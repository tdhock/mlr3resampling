\name{proj_submit}
\alias{proj_submit}
\title{
  Submit resampling split jobs in parallel
}
\description{
  Before running this function, you should define
  \code{cluster.functions} in your \code{~/.batchtools.conf.R} file.
  It makes a batchtools registry, then runs
  \code{batchtools::batchMap()} and \code{batchtools::submitJobs()};
  each iteration runs \code{\link{proj_compute_until_done}}.
}
\usage{
proj_submit(
  proj_dir, tasks = 2, hours = 1, gigabytes = 1,
  verbose = FALSE, cluster.functions = NULL)
}
\arguments{
  \item{proj_dir}{Project directory created via \code{\link{proj_grid}}.}
  \item{tasks}{Positive integer: number of batchtools jobs, translated
    into one SLURM job array with this number of tasks.}
  \item{hours}{Hours of walltime to ask the SLURM scheduler.}
  \item{gigabytes}{Gigabytes of memory to ask the SLURM scheduler.}
  \item{verbose}{Logical: print messages?}
  \item{cluster.functions}{Cluster functions from \code{batchtools},
    useful for testing.}
}
\details{
  This is Step 2 out of the
  typical 3 step pipeline (init grid, submit, read results).
}
\value{
  The batchtools registry.
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
mlr3resampling::proj_submit(pkg.proj.dir)
batchtools::waitForJobs()
fread(file.path(pkg.proj.dir, "results.csv"))

}
