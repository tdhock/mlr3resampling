\name{proj_compute}
\alias{proj_compute}
\alias{proj_compute_until_done}
\title{
  Compute resampling results in a project
}
\description{
  \code{proj_compute()} looks in \code{grid_jobs.csv} for a row with
  \code{status=="not started"}, and picks the first one to work on,
  updating \code{status="started"}.
  After having run \code{train()} and \code{predict()}, a data table
  with one row is saved to an RDS file in the \code{grid_jobs}
  directory, and \code{status="done"} is updated.
  \code{proj_compute_until_done()} keeps doing that in a while loop.
}
\details{
  If everything goes well, the user should not need to run this
  function.
  Instead, the user runs \code{\link{proj_submit}} as Step 2 out of the
  typical 3 step pipeline (init grid, submit, read results).
  \code{proj_compute} can sometimes be useful for testing or debugging the submit step,
  since it runs one split at a time.
}
\usage{
proj_compute(proj_dir, verbose = FALSE)
proj_compute_until_done(proj_dir, verbose = FALSE)
proj_fread(proj_dir)
}
\arguments{
  \item{proj_dir}{Project directory created by \code{\link{proj_grid}}.}
  \item{verbose}{Logical: print messages?}
}
\value{
  \code{proj_compute} returns a data table with one row of results, and
  \code{proj_compute_until_done} returns same result as
  \code{\link{proj_fread}}.
}
\author{
  Toby Dylan Hocking
}
\examples{

N <- 80
library(data.table)
set.seed(1)
reg.dt <- data.table(
  x=runif(N, -2, 2),
  person=factor(rep(c("Alice","Bob"), each=0.5*N)))
reg.pattern.list <- list(
  easy=function(x, person)x^2,
  impossible=function(x, person)(x^2)*(-1)^as.integer(person))
SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
reg.task.list <- list()
for(pattern in names(reg.pattern.list)){
  f <- reg.pattern.list[[pattern]]
  task.dt <- data.table(reg.dt)[
  , y := f(x,person)+rnorm(N, sd=0.5)
  ][]
  task.obj <- mlr3::TaskRegr$new(
    pattern, task.dt, target="y")
  task.obj$col_roles$feature <- "x"
  task.obj$col_roles$stratum <- "person"
  task.obj$col_roles$subset <- "person"
  reg.task.list[[pattern]] <- task.obj
}
reg.learner.list <- list(
  featureless=mlr3::LearnerRegrFeatureless$new())
if(requireNamespace("rpart")){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}

pkg.proj.dir <- tempfile()
mlr3resampling::proj_grid(
  pkg.proj.dir,
  reg.task.list,
  reg.learner.list,
  SOAK,
  order_jobs = function(DT)1:2, # for CRAN.
  score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
mlr3resampling::proj_compute(pkg.proj.dir)
fread(file.path(pkg.proj.dir, "grid_jobs.csv"))
mlr3resampling::proj_compute_until_done(pkg.proj.dir)
fread(file.path(pkg.proj.dir, "grid_jobs.csv"))

}
