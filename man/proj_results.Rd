\name{proj_results}
\alias{proj_results}
\alias{proj_results_save}
\alias{proj_fread}
\title{
  Combine and save results in a project
}
\description{
  \code{proj_results} globs the RDS result files in the project
  directory, and combines them into a result table via \code{rbindlist()}.
  \code{proj_results_save} saves that result table to \code{results.rds}
  and one or more CSV files (redundant with RDS data, but more
  convenient).
  \code{proj_fread} reads the CSV files, adding columns from
  \code{results.csv} to \code{learners*.csv}.
}
\usage{
proj_results(proj_dir, verbose=FALSE)
proj_results_save(proj_dir, verbose=FALSE)
proj_fread(proj_dir)
}
\arguments{
  \item{proj_dir}{Project directory created via
    \code{\link{proj_grid}}.}
  \item{verbose}{logical: cat progress? (default FALSE)}
}
\details{
  This is Step 3 out of the typical 3 step pipeline (init grid, submit, read results).
  Actually, if step 2 worked as intended, then
  \code{\link{proj_results_save}} is called at the end of step 2,
  which saves result files to disk that you can read directly:
  \describe{
    \item{\code{results.csv}}{contains test measures for each split.}
    \item{\code{results.rds}}{contains additional list columns for \code{learner}
      and \code{pred} (useful for model interpretation), and can be read via
      \code{readRDS()}}
    \item{\code{learners.csv}}{only exists if \code{learner} column is a
      data frame, in which case it contains the atomic columns, along
      with meta-data describing each split.}
    \item{\code{learners_*.csv}}{only exists if \code{learner} column is
      a named list of data frames: star in file name is expanded using
      list names, with CSV data taken from atomic columns.}
  }
}
\value{
  \code{proj_results} returns a data table with all columns, whereas
  \code{proj_results_save} returns the same table with only atomic columns.
  \code{proj_fread} returns a list
  with names corresponding to CSV files in the test directory, and
  values are the data tables that result from \code{\link[data.table:fread]{fread}}.
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
  noise=runif(N, -2, 2),
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
  task.obj$col_roles$feature <- c("x","noise")
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
  save_learner=function(L){
    if(inherits(L, "LearnerRegrRpart")){
      list(rpart=L$model$frame)
    }
  },
  order_jobs = function(DT)which(DT$iteration==1)[1:2], # for CRAN.
  score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
computed <- mlr3resampling::proj_compute_all(pkg.proj.dir)
result_list <- mlr3resampling::proj_fread(pkg.proj.dir)
result_list$learners_rpart.csv # one row per node in decision tree.
result_list$results.csv # test error in regr.* columns.

}
