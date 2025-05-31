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

test_that("proj_compute_until_done", {
  pkg.proj.dir <- tempfile()
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    SOAK,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  mlr3resampling::proj_compute_until_done(pkg.proj.dir, verbose=TRUE)    
  result_dt <- fread(file.path(pkg.proj.dir, "results.csv"))
  expect_equal(nrow(result_dt), 72)
})

test_submit <- function(cluster.functions){
  pkg.proj.dir <- tempfile()
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    SOAK,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  mlr3resampling::proj_submit(
    pkg.proj.dir,
    cluster.functions=cluster.functions,
    verbose=TRUE)    
  batchtools::waitForJobs(timeout=60)
  system(paste("cat", file.path(pkg.proj.dir, "registry", "logs", "*")))
  fread(file.path(pkg.proj.dir, "results.csv"))
}

test_that("proj_submit Interactive", {
  result_dt <- test_submit(batchtools::makeClusterFunctionsInteractive())
  expect_equal(nrow(result_dt), 72)
})

test_that("proj_submit Multicore", {
  result_dt <- test_submit(batchtools::makeClusterFunctionsMulticore())
  expect_equal(nrow(result_dt), 72)
})

test_that("proj_submit SLURM", {
  tmpl <- system.file("slurm-afterok.tmpl", package="mlr3resampling")
  result_dt <- test_submit(batchtools::makeClusterFunctionsSlurm(tmpl))
  expect_equal(nrow(result_dt), 72)
})
