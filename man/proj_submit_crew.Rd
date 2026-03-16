\name{proj_submit_crew}
\alias{proj_submit_crew}
\alias{proj_compute_crew}
\title{
  Compute jobs using crew.cluster (SLURM job arrays) or crew (local)
}
\description{
  \code{proj_submit_crew} submits jobs to SLURM using job arrays via
  the \pkg{crew.cluster} package. Unlike \code{\link{proj_submit}} which uses MPI
  (requiring all CPUs to be available simultaneously), job arrays allow
  each task to launch independently as resources become available.

  \code{proj_compute_crew} computes jobs locally using the \pkg{crew} package
  for parallel processing without SLURM.
}
\usage{
proj_submit_crew(
  proj_dir,
  workers = 10,
  hours = 1,
  gigabytes = 4,

  seconds_idle = 600,
  verbose = FALSE,
  ...)

proj_compute_crew(
  proj_dir,
  workers = parallel::detectCores(),
  seconds_idle = 120,
  verbose = FALSE)
}
\arguments{
  \item{proj_dir}{Project directory created via \code{\link{proj_grid}}.}
  \item{workers}{Positive integer: number of parallel workers. For SLURM,
    this is the job array size.}
  \item{hours}{Hours of walltime to ask the SLURM scheduler.}
  \item{gigabytes}{Gigabytes of memory per CPU to ask the SLURM scheduler.}
  \item{seconds_idle}{Seconds to keep workers alive while idle waiting for
    new tasks. After this time, idle workers terminate.}
  \item{verbose}{Logical: print progress messages?}
  \item{...}{Additional arguments passed to
    \code{crew.cluster::crew_controller_slurm}.}
}
\details{
  These functions provide an alternative to the MPI-based
  \code{\link{proj_submit}} and \code{\link{proj_compute_mpi}} functions.

  \strong{Key advantages of crew.cluster job arrays:}
  \itemize{
    \item Tasks launch independently as resources become available (no need
      to wait for all CPUs simultaneously)
    \item O(1) submission time vs O(n) for individual jobs
    \item 30-50\% reduction in SLURM scheduler overhead
    \item Supports scaling to thousands of parallel workers
    \item No MPI installation required
  }

  \code{proj_submit_crew} is the recommended approach for large-scale SLURM
  cluster computing. \code{proj_compute_crew} is useful for local parallel
  computing or testing without SLURM.

  Both functions automatically call \code{\link{proj_results_save}} after
  all jobs complete.
}
\value{
  Both functions return invisibly a data table with results from all
  computed jobs. They also save results to CSV files in the project
  directory via \code{proj_results_save}.
}
\author{
Toby Dylan Hocking
}
\seealso{
  \code{\link{proj_submit}} for the MPI-based approach,
  \code{\link{proj_grid}} for creating project grids,
  \code{\link{proj_results}} for reading results.
}
\examples{
\dontrun{
# First, create a project grid (see ?proj_grid for details)
# Then submit using crew.cluster job arrays:

# SLURM cluster submission with job arrays
proj_submit_crew(
  proj_dir = "my_project",
  workers = 50,        # 50 parallel workers

  hours = 2,           # 2 hours walltime
  gigabytes = 4,       # 4GB per worker
  seconds_idle = 600,  # Keep workers alive 10 minutes
  verbose = TRUE
)

# Local parallel computing (no SLURM needed)
proj_compute_crew(
  proj_dir = "my_project",
  workers = 4,         # Use 4 local cores
  verbose = TRUE
)

# Read results after completion
results <- mlr3resampling::proj_fread("my_project")
}
}
