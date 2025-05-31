\name{proj_results}
\alias{proj_results}
\alias{proj_results_save}
\title{
  Combine and save results in a project
}
\description{
  \code{proj_results} globs the RDS result files in the project
  directory, and combines them into a result table via \code{rbindlist()}.
  \code{proj_results_save} saves that result table to \code{results.rds}
  and \code{results.csv}.
}
\usage{
proj_results(proj_dir)
proj_results_save(proj_dir)
}
\arguments{
  \item{proj_dir}{Project directory created via \code{\link{proj_grid}}.}
}
\details{
  This is Step 3 out of the typical 3 step pipeline (init grid, submit, read results).
  Actually, if step 2 worked as intended, the last \code{\link{proj_compute}}
  calls \code{\link{proj_results_save}}, which saves two result files to disk
  that you can read directly:
  \begin{itemize}
  \item \code{results.csv} contains test measures for each split, and
  can be read via \code{fread()}.
  \item \code{results.rds} contains additional list columns for \code{learner}
  and \code{pred} (useful for model interpretation), and can be read via
  \code{readRDS()}.
  \end{itemize}
}
\value{
  \code{proj_results} returns a data table, whereas
  \code{proj_results_save} returns nothing (side effect: save result
  files).
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
  score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
mlr3resampling::proj_compute_until_done(pkg.proj.dir)
fread(file.path(pkg.proj.dir, "results.csv"))

}
