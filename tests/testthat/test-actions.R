library(testthat)
library(data.table)

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
if(requireNamespace("rpart")){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}

test_that("submit works", {
  pkg.proj.dir <- if(interactive())"~/mlr3resampling-test" else tempfile()
  unlink(pkg.proj.dir, recursive = TRUE)
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    SOAK,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  results.csv <- file.path(pkg.proj.dir, "results.csv")
  expect_false(file.exists(results.csv))
  job.id <- mlr3resampling::proj_submit(pkg.proj.dir, verbose=TRUE)
  squeue.cmd <- paste0("squeue -h -j", job.id)
  while(length(system(squeue.cmd, intern=TRUE))){
    cat("waiting for SLURM job", job.id, "\n")
    Sys.sleep(1)
  }
  result_dt <- fread(results.csv)
  expect_equal(nrow(result_dt), 72)
})
