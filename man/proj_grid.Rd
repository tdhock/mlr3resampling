\name{proj_grid}
\alias{proj_grid}
\title{
  Initialize a new project grid table
}
\description{
  A project grid consists of all combinations of tasks, learners,
  resampling types, and resampling iterations, to be computed in parallel.
  This function creates a project directory with files to describe the grid.
}
\usage{
proj_grid(
  proj_dir, tasks, learners, resamplings,
  order_jobs = NULL, score_args = NULL,
  save_learner = save_learner_default, save_pred = FALSE)
}
\arguments{
  \item{proj_dir}{Path to directory to create.}
  \item{tasks}{List of Tasks, or a single Task.}
  \item{learners}{List of Learners, or a single Learner.}
  \item{resamplings}{List of Resamplings, or a single Resampling.}
  \item{order_jobs}{Function which takes split table as input, and
    returns integer vector of row numbers of the split table to write to
    \code{grid_jobs.csv}, which is how worker processes determine what
    work to do next (smaller numbers have higher priority). Default
    \code{NULL} means to keep default order.}
  \item{score_args}{Passed to \code{pred$score()}.}
  \item{save_learner}{Function to process Learner, after
    training/prediction, but before saving result to disk.
    For interpreting complex models, you should write a function that
    returns only the parts of the model that you need (and discards the
    other parts which would take up disk space for no reason).
    Default is to call \code{save_learner} method if it exists.
    \code{FALSE} means to not keep it (always returns \code{NULL}).
    \code{TRUE} means to keep it without any special processing.
  }
  \item{save_pred}{Function to process Prediction before saving to disk.
    Default \code{FALSE} means to not keep it (always returns
    \code{NULL}).
    \code{TRUE} means to keep it without any special processing.}
}
\details{
  This is Step 1 out of the
  typical 3 step pipeline (init grid, submit, read results).
  It creates a \code{grid_jobs.csv} table which has a column \code{status};
  each row is initialized to \code{"not started"} or \code{"done"},
  depending on whether the corresponding result RDS file exists already.
}
\value{
Data table of splits to be processed (same as table saved to \code{grid_jobs.rds}).
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
mlr3resampling::proj_compute(2, pkg.proj.dir)

}
