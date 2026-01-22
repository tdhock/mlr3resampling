library(testthat)
library(data.table)

# Setup test data (same as test-SLURM.R)
N <- 80
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
if(requireNamespace("rpart", quietly=TRUE)){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}

test_that("proj_submit_crew exists and validates inputs", {
  skip_if_not_installed("crew")
  skip_if_not_installed("crew.cluster")

  # Function exists
  expect_true(exists("proj_submit_crew", where=asNamespace("mlr3resampling")))

  # Invalid proj_dir should error (message differs on Windows vs Unix)
  expect_error(
    mlr3resampling::proj_submit_crew("/nonexistent/path")
  )
})

test_that("proj_compute_crew exists and validates inputs", {
  skip_if_not_installed("crew")

  # Function exists
  expect_true(exists("proj_compute_crew", where=asNamespace("mlr3resampling")))

  # Invalid proj_dir should error (message differs on Windows vs Unix)
  expect_error(
    mlr3resampling::proj_compute_crew("/nonexistent/path")
  )
})

test_that("proj_submit_crew grid setup works", {
  skip_if_not_installed("crew")
  skip_if_not_installed("crew.cluster")

  pkg.proj.dir <- tempfile()
  unlink(pkg.proj.dir, recursive = TRUE)
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    SOAK,
    order_jobs = function(DT)1:2, # limit jobs for test
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))

  # Verify proj_dir exists
  expect_true(dir.exists(pkg.proj.dir))

  # Verify proj_todo returns jobs to compute
  todo <- mlr3resampling::proj_todo(pkg.proj.dir)
  expect_true(length(todo) > 0)

  # Clean up
  unlink(pkg.proj.dir, recursive = TRUE)
})

test_that("proj_compute_crew handles empty todo list", {
  skip_on_cran()
  skip_if_not_installed("crew")

  pkg.proj.dir <- tempfile()
  unlink(pkg.proj.dir, recursive = TRUE)
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    SOAK,
    order_jobs = function(DT)1:2,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))

  # First compute all jobs using regular method
  mlr3resampling::proj_compute_all(pkg.proj.dir, verbose=FALSE)

  # Second run - should message about no jobs
  expect_message(
    mlr3resampling::proj_compute_crew(pkg.proj.dir, workers=2, verbose=FALSE),
    "No jobs to compute"
  )

  # Clean up
  unlink(pkg.proj.dir, recursive = TRUE)
})

test_that("proj_submit_crew on SLURM cluster", {
  skip_on_cran()
  skip_if_not_installed("crew")
  skip_if_not_installed("crew.cluster")
  # Skip if SLURM is not available (check for squeue command)
  skip_if(system("which squeue", ignore.stdout=TRUE, ignore.stderr=TRUE) != 0,
          "SLURM not available")
  # Also skip if package is not properly installed (devtools::test scenario)
  skip_if(!requireNamespace("mlr3resampling", quietly=TRUE) || 
          !("proj_compute" %in% ls(asNamespace("mlr3resampling"))),
          "Package not installed (running in devtools::test)")

  pkg.proj.dir <- if(interactive())"~/mlr3resampling-crew-test" else tempfile()
  unlink(pkg.proj.dir, recursive = TRUE)
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    SOAK,
    order_jobs = function(DT)1:4, # limit jobs for test
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))

  results.csv <- file.path(pkg.proj.dir, "results.csv")
  expect_false(file.exists(results.csv))

  # Submit using crew.cluster job arrays
  mlr3resampling::proj_submit_crew(
    pkg.proj.dir,
    workers = 2,
    hours = 1,
    gigabytes = 1,
    seconds_idle = 120,
    verbose = TRUE
  )

  # Verify results
  expect_true(file.exists(results.csv))
  result_dt <- fread(results.csv)
  expect_equal(nrow(result_dt), 4)

  # Clean up
  unlink(pkg.proj.dir, recursive = TRUE)
})
