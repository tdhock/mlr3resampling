\name{proj_compute}
\alias{proj_compute}
\title{
  Compute resampling results in a project
}
\description{
  Runs \code{train()} and \code{predict()}, then a data table
  with one row is saved to an RDS file in the \code{grid_jobs}
  directory.
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
proj_compute(grid_job_i, proj_dir, verbose=FALSE, process_fun=Sys.getpid)
}
\arguments{
  \item{grid_job_i}{integer from 1 to number of jobs (rows in \code{grid_jobs.csv}).}
  \item{proj_dir}{Project directory created by \code{\link{proj_grid}}.}
  \item{verbose}{Logical: print messages?}
  \item{process_fun}{function called with no arguments
    (default \code{\link{Sys.getpid}}),
    should return integer process id number.
  }
}
\value{
  \code{proj_compute} returns a data table with one row of results.
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
mlr3resampling::proj_compute(1, pkg.proj.dir)

}
