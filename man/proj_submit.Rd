\name{proj_submit}
\alias{proj_submit}
\alias{proj_compute_mpi}
\alias{proj_compute_all}
\alias{proj_todo}
\title{
  Compute several resampling jobs
}
\description{
  \code{proj_todo} determines which jobs remain to be computed.
  \code{proj_compute_all} computes all remaining jobs sequentially,
  whereas \code{proj_compute_mpi} computes them in parallel using MPI (should
  be run in an R session activated by \code{mpirun} or \code{srun}).
  \code{proj_submit} is a non-blocking call to SLURM \code{sbatch},
  asking for a single job with several tasks that run \code{proj_compute_mpi}.
}
\usage{
proj_todo(proj_dir)
proj_compute_mpi(proj_dir, verbose=FALSE)
proj_compute_all(proj_dir, verbose=FALSE)
proj_submit(
  proj_dir, tasks = 2, hours = 1, gigabytes = 1,
  verbose = FALSE)
}
\arguments{
  \item{proj_dir}{Project directory created via \code{\link{proj_grid}}.}
  \item{tasks}{Positive integer: \code{ntasks} parameter for SLURM scheduler, one for
    manager, others are workers.}
  \item{hours}{Hours of walltime to ask the SLURM scheduler.}
  \item{gigabytes}{Gigabytes of memory to ask the SLURM scheduler.}
  \item{verbose}{Logical: print messages?}
}
\details{
  This is Step 2 out of the
  typical 3 step pipeline (init grid, submit, read results).
}
\value{
  \code{proj_submit} returns the ID of the submitted SLURM job.
  \code{proj_compute_all} and \code{proj_compute_mpi} return a data
  table of results computed.
  \code{proj_todo} returns a vector of job IDs not yet computed.
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
mlr3resampling::proj_compute_all(pkg.proj.dir)

}
