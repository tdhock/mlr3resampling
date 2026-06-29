library(testthat)
library(data.table)
if(requireNamespace("lgr"))lgr::get_logger("mlr3")$set_threshold("warn")

N <- 2100
abs.x <- 20
set.seed(1)
x.vec <- sort(runif(N, -abs.x, abs.x))
task.dt <- data.table(
  x=x.vec,
  y = sin(x.vec)+rnorm(N,sd=0.5))
atomic.group.size <- 2
task.dt[, agroup := rep(seq(1, N/atomic.group.size), each=atomic.group.size)]
task.dt[, random_group := rep(
  rep(c("A","B","B","C","C","C","C"), each=atomic.group.size),
  l=.N
)]
group.tab <- table(task.dt$random_group)
get_props <- function(x)x/sum(x)
prop.tab <- get_props(group.tab)
get_prop_mat <- function(ilist){
  sapply(ilist, function(i)get_props(table(task.dt[i, random_group])))
}
test_that("ResamplingSameOtherSizesCV no subset, no group, no stratum", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- -1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  expect_equal(computed[["test.fold"]], 1:n.folds)
  full.train.size <- N*(n.folds-1)/n.folds
  expect_equal(computed[["n.train.groups"]], rep(full.train.size, n.folds))
})
test_that("ResamplingSameOtherSizesCV no subset, yes group, no stratum", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- -1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  expect_equal(computed[["test.fold"]], 1:n.folds)
  full.train.size <- N*(n.folds-1)/n.folds
  expect_equal(computed[["n.train.groups"]], rep(full.train.size/atomic.group.size, n.folds))
  expect_equal(sapply(computed[["train"]], length), rep(full.train.size, n.folds))
  expected.props <- matrix(
    prop.tab, length(prop.tab), n.folds, dimnames=list(names(prop.tab),NULL))
  computed.train <- get_prop_mat(computed[["train"]])
  expect_false(identical(computed.train, expected.props))
  computed.test <- get_prop_mat(computed[["test"]])
  expect_false(identical(computed.test, expected.props))
})
test_that("ResamplingSameOtherSizesCV no subset, yes group, yes stratum", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- -1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  expect_equal(computed[["test.fold"]], 1:n.folds)
  full.train.size <- N*(n.folds-1)/n.folds
  expect_equal(computed[["n.train.groups"]], rep(full.train.size/atomic.group.size, n.folds))
  expect_equal(sapply(computed[["train"]], length), rep(full.train.size, n.folds))
  expected.props <- matrix(
    prop.tab, length(prop.tab), n.folds, dimnames=list(names(prop.tab),NULL))
  computed.train <- get_prop_mat(computed[["train"]])
  expect_identical(computed.train, expected.props)
  computed.test <- get_prop_mat(computed[["test"]])
  expect_identical(computed.test, expected.props)
})
test_that("ResamplingSameOtherSizesCV yes subset, yes group, yes stratum, ignore_subset", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$subset <- "random_group"
  n.subsets <- length(unique(task.dt$random_group))
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- -1
  same_other_sizes_cv$param_set$values$ignore_subset <- TRUE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  ## same as no subset.
  expect_equal(computed[["test.fold"]], 1:n.folds)
  full.train.size <- N*(n.folds-1)/n.folds
  expect_equal(computed[["n.train.groups"]], rep(full.train.size/atomic.group.size, n.folds))
  expect_equal(sapply(computed[["train"]], length), rep(full.train.size, n.folds))
  expected.props <- matrix(
    prop.tab, length(prop.tab), n.folds, dimnames=list(names(prop.tab),NULL))
  computed.train <- get_prop_mat(computed[["train"]])
  expect_identical(computed.train, expected.props)
  computed.test <- get_prop_mat(computed[["test"]])
  expect_identical(computed.test, expected.props)
})
test_that("ResamplingSameOtherSizesCV no subset, yes group, yes stratum, sizes=0", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- 0
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  expect_equal(computed[["test.fold"]], 1:n.folds)
  full.train.size <- N*(n.folds-1)/n.folds
  expect_equal(computed[["n.train.groups"]], rep(full.train.size/atomic.group.size, n.folds))
  expect_equal(sapply(computed[["train"]], length), rep(full.train.size, n.folds))
  expected.props <- matrix(
    prop.tab, length(prop.tab), n.folds, dimnames=list(names(prop.tab),NULL))
  computed.train <- get_prop_mat(computed[["train"]])
  expect_identical(computed.train, expected.props)
  computed.test <- get_prop_mat(computed[["test"]])
  expect_identical(computed.test, expected.props)
})
test_that("ResamplingSameOtherSizesCV no subset, yes group, yes stratum, sizes=1", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- 1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  expect_equal(computed[["test.fold"]], rep(1:n.folds,each=2))
  full.train.size <- N*(n.folds-1)/n.folds
  expected.n <- (full.train.size/atomic.group.size)/c(2,1)
  expect_equal(computed[["n.train.groups"]], rep(expected.n, n.folds))
  expected.props <- matrix(
    prop.tab, length(prop.tab), n.folds*2, dimnames=list(names(prop.tab),NULL))
  computed.test <- get_prop_mat(computed[["test"]])
  expect_identical(computed.test, expected.props)
})
test_that("ResamplingSameOtherSizesCV yes subset, yes group, yes stratum", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$subset <- "random_group"
  n.subsets <- length(unique(task.dt$random_group))
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- -1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  expected.subsets <- list(all=c("A","B","C"),other=c("B","C"),same="A")
  expect_equal(nrow(computed), n.folds*n.subsets*length(expected.subsets))
  three <- computed[
    test.fold==1 & seed==1 & test.subset=="A"
  ][order(train.subsets)]
  expect_equal(three[["train.subsets"]], names(expected.subsets))
  expect_identical(three[["test"]][[1]], three[["test"]][[2]])
  expect_identical(three[["test"]][[1]], three[["test"]][[3]])
  exp.prop.list <- unname(lapply(expected.subsets, function(N)get_props(group.tab[N,drop=FALSE])))
  three.prop.list <- get_prop_mat(three[["train"]])
  expect_identical(three.prop.list, exp.prop.list)
})

test_that("ResamplingSameOtherSizesCV yes subset, yes group, yes stratum, sizes=0", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$subset <- "random_group"
  n.subsets <- length(unique(task.dt$random_group))
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  same_other_sizes_cv$param_set$values$folds <- 3
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$sizes <- 0
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt[
    test.fold == 1 & test.subset == "A",
    .(train.subsets, groups, n.train.groups)
  ][order(train.subsets, n.train.groups)]
  expected <- data.table(
    train.subsets = c("all", "all", "other", "other", "same"),
    groups = c(700L, 700L, 600L, 600L, 100L),
    n.train.groups = c(100L, 700L, 100L, 600L, 100L))
  expect_identical(computed, expected)
})

test_that("ResamplingSameOtherSizesCV yes subset, yes group, yes stratum, sizes=1", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$subset <- "random_group"
  n.subsets <- length(unique(task.dt$random_group))
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- 1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  n.train.per.test <- 9
  expect_equal(nrow(computed), n.folds*n.subsets*n.train.per.test)
})
test_that("ResamplingSameOtherSizesCV yes subset, yes group, yes stratum, sizes=2", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$subset <- "random_group"
  n.subsets <- length(unique(task.dt$random_group))
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- 2
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  n.train.per.test <- 12
  expect_equal(nrow(computed), n.folds*n.subsets*n.train.per.test)
})

test_that("ResamplingSameOtherSizesCV yes subset, yes group, yes stratum, sizes=2, same/other", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$group <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$subset <- "random_group"
  n.subsets <- length(unique(task.dt$random_group))
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- -1
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$param_set$values$subsets <- "SO"
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  n.train.per.test <- 2
  expect_equal(nrow(computed), n.folds*n.subsets*n.train.per.test)
})

test_that("hjust correct for two algos far apart", {
  bench.score <- rbind(
    data.table(
      task_id="easy",
      n.train.groups=1,
      groups=1,
      test.subset=1,
      algorithm="featureless",
      test.fold=c(1,2,1,2,1,2),
      regr.rmse=c(24.1,25,26.2,27,28.3,29)+100,
      train.subsets=c("same","same","other","other","all","all")),
    data.table(
      task_id="easy",
      n.train.groups=1,
      groups=1,
      test.subset=1,
      algorithm="rpart",
      test.fold=c(1,2,1,2,1,2),
      regr.rmse=c(8.1,9,12.2,13,22.3,23),
      train.subsets=c("all","all","other","other","same","same")))
  bench.plist <- mlr3resampling::pvalue(bench.score)
  expect_null(bench.plist$pvalues[["diff_mean"]])
  expect_equal(bench.plist$pvalues[algorithm=="featureless", hjust], c(1,1))
  expect_equal(bench.plist$pvalues[algorithm=="rpart", hjust], c(0,0))
  expect_equal(bench.plist$stats[algorithm=="featureless", hjust], c(1,1,1))
  expect_equal(bench.plist$stats[algorithm=="rpart", hjust], c(0,0,0))
})

test_that("hjust=0.5 for algo in middle", {
  score_dt <- rbind(
    data.table(
      task_id="test",
      n.train.groups=1,
      groups=1,
      test.subset="foo",
      train.subsets="same",
      test.fold=1:3,
      algorithm="featureless",
      classif.auc=0.5),
    data.table(
      task_id="test",
      n.train.groups=1,
      groups=1,
      test.subset="foo",
      train.subsets="same",
      test.fold=1:3,
      algorithm="conv",
      classif.auc=c(0.91,0.915, 0.93)),
    data.table(
      task_id="test",
      n.train.groups=1,
      groups=1,
      test.subset="foo",
      train.subsets="other",
      test.fold=1:3,
      algorithm="conv",
      classif.auc=c(0.71,0.715, 0.72)))
  plist <- mlr3resampling::pvalue(score_dt)
  expect_equal(plist$stats[algorithm=="featureless", hjust], 0)
  expect_equal(plist$stats[algorithm=="conv" & Train_subsets=="other", hjust], 0.5)
  expect_equal(plist$stats[algorithm=="conv" & Train_subsets=="same", hjust], 1)
})

test_that("plot.pvalue keeps blank top level", {
  blank.top.levels <- c("all", "all-same", "same", "other-same", "other", "")
  N <- 60L
  task.dt <- data.table(
    x=runif(N, -2, 2),
    subset_col=factor(rep(c("A", "B"), each=N/2))
  )[, y := x^2 + rnorm(.N, sd=0.2)][]
  soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  reg.task <- mlr3::TaskRegr$new("toy_plot", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$subset <- "subset_col"
  reg.task$col_roles$stratum <- "subset_col"
  soak$param_set$values$folds <- 2L
  soak$param_set$values$seeds <- 1L
  soak$param_set$values$sizes <- 0L
  bench.grid <- mlr3::benchmark_grid(
    reg.task,
    mlr3::LearnerRegrFeatureless$new(),
    soak
  )
  bench.result <- mlr3::benchmark(bench.grid)
  soak.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
  plist <- mlr3resampling::pvalue(soak.score)
  expect_identical(
    levels(plist$stats$Train_subsets),
    blank.top.levels)
})

test_that("plot.pvalue_downsample keeps blank top level", {
  blank.top.levels <- c("all", "all-same", "same", "other-same", "other", "")
  N <- 60L
  subset_name <- "Female cohort with long text"
  task.dt <- data.table(
    x=runif(N, -2, 2),
    subset_col=factor(rep(c(subset_name, "Male cohort with long text"), each=N/2))
  )[, y := x^2 + rnorm(.N, sd=0.2)][]
  soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  reg.task <- mlr3::TaskRegr$new("toy_soak", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$subset <- "subset_col"
  reg.task$col_roles$stratum <- "subset_col"
  soak$param_set$values$folds <- 2L
  soak$param_set$values$seeds <- 1L
  soak$param_set$values$sizes <- 0L
  bench.grid <- mlr3::benchmark_grid(
    reg.task,
    mlr3::LearnerRegrFeatureless$new(),
    soak
  )
  bench.result <- mlr3::benchmark(bench.grid)
  soak.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
  down.list <- mlr3resampling::pvalue_downsample(
    soak.score[test.subset == subset_name]
  )
  expect_identical(
    levels(down.list$stats$Train_subsets),
    blank.top.levels)
})

test_that("regular K fold CV works in proj", {
  N <- 80
  set.seed(1)
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(c("Alice","Bob"), each=0.5*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3::ResamplingCV$new()
  kfold$param_set$values$folds <- 2
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
  expect_error({
    mlr3resampling::proj_grid(
      pkg.proj.dir,
      list(),
      reg.learner.list,
      kfold,
      score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  }, "tasks is empty, but need at least one")
  expect_error({
    mlr3resampling::proj_grid(
      pkg.proj.dir,
      reg.task.list,
      list(),
      kfold,
      score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  }, "learners is empty, but need at least one")
  expect_error({
    mlr3resampling::proj_grid(
      pkg.proj.dir,
      reg.task.list,
      reg.learner.list,
      list(),
      score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  }, "resamplings is empty, but need at least one")
  grid_dt <- mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    save_learner = TRUE,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  todo.i <- mlr3resampling::proj_todo(pkg.proj.dir)
  todo.expected <- 1:nrow(grid_dt)
  expect_identical(todo.i, todo.expected)
  row2 <- mlr3resampling::proj_compute(2, pkg.proj.dir)
  expect_equal(nrow(row2), 1)
  todo.i <- mlr3resampling:::proj_todo(pkg.proj.dir)
  expect_identical(todo.i, todo.expected[-2])
  row3 <- mlr3resampling::proj_compute(3, pkg.proj.dir)
  two_rows <- mlr3resampling::proj_results(pkg.proj.dir)
  expect_equal(nrow(two_rows), 2)
  expect_identical(two_rows$process, rep(Sys.getpid(), 2))
  future::plan("multisession", workers=2)
  all_dt <- mlr3resampling::proj_compute_all(pkg.proj.dir)
  expect_equal(sum(all_dt$process!=Sys.getpid()), 6)
  expect_equal(length(unique(all_dt$process)), 2)
  future::plan("sequential")
  expect_false(file.exists(file.path(pkg.proj.dir, "learners.csv")))
  results_dt <- fread(file.path(pkg.proj.dir, "results.csv"))
  expect_false(identical(results_dt$regr.mae[1], results_dt$regr.mae[2]))
  expect_equal(nrow(results_dt), 8)
  expect_error({
    mlr3resampling::proj_grid(
      pkg.proj.dir,
      reg.task.list,
      reg.learner.list,
      kfold)
  }, "already exists, so not over-writing")
  pkg.proj.dir <- tempfile()
  expect_warning({
    mlr3resampling::proj_grid(
      pkg.proj.dir,
      reg.task.list,
      reg.learner.list,
      kfold)
  }, "no score_args nor save_pred, so there will be no test error results")
})

test_that("proj_test down-samples proportionally", {
  N <- 8000
  set.seed(1)
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(c("Alice","Bob"), c(0.1,0.9)*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3::ResamplingCV$new()
  kfold$param_set$values$folds <- 2
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
  keep_cols <- c("var","dev","n")
  pgrid <- mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    save_learner=function(L){
      if(inherits(L, "LearnerRegrRpart")){
        list(rpart=L$model$frame[,keep_cols])
      }
    },
    save_pred = TRUE,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  out_list <- mlr3resampling::proj_test(pkg.proj.dir)
  expect_identical(names(out_list), c("grid_jobs.csv", "learners_rpart.csv", "results.csv"))
  expect_identical(out_list$grid_jobs.csv$iteration, rep(1L, 4))
  test_dir <- file.path(pkg.proj.dir, "test")
  test.task <- readRDS(file.path(test_dir, "tasks", "1.rds"))
  count.tab <- table(test.task$data(cols="person"))
  expect_equal(as.numeric(count.tab), c(10, 90))
  test_res_dt <- readRDS(file.path(test_dir, "results.rds"))
  expect_equal(length(test_res_dt$pred[[1]]$row_ids), 50)
  learners_dt <- fread(file.path(test_dir, "learners_rpart.csv"))
  expected_csv_cols <- c("grid_job_i", "var", "dev", "n")
  expect_identical(names(learners_dt), expected_csv_cols)
  out_dt_list <- mlr3resampling::proj_fread(test_dir)
  expected_join_cols <- c(
    expected_csv_cols,
    "task_id", "learner_id", "resampling_id", "iteration")
  expect_identical(names(out_dt_list$learners_rpart.csv), expected_join_cols)
  rpart.job.i.vec <- which(pgrid$learner_id=="regr.rpart")
  mlr3resampling::proj_compute(rpart.job.i.vec[1], pkg.proj.dir)
  mlr3resampling::proj_compute(rpart.job.i.vec[length(rpart.job.i.vec)], pkg.proj.dir)
  mlr3resampling::proj_results_save(pkg.proj.dir)
  csv_dt_list <- mlr3resampling::proj_fread(pkg.proj.dir)
  rpart_dt <- csv_dt_list[["learners_rpart.csv"]]
  expect_identical(names(rpart_dt), expected_join_cols)
  expect_equal(sum(is.na(rpart_dt[["task_id"]])), 0)
  expect_equal(sum(is.na(rpart_dt[["learner_id"]])), 0)
  expect_equal(sum(is.na(rpart_dt[["resampling_id"]])), 0)
  expect_equal(sum(is.na(rpart_dt[["iteration"]])), 0)
})

last_lev <- function(x){
  levs <- levels(factor(x))
  levs[length(levs)]
}

test_that("set works after score(), other is last Y level", {
  N <- 80
  set.seed(1)
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=rep(1:2, each=0.5*N))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^person)
  SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  reg.task.list <- list()
  for(pattern in names(reg.pattern.list)){
    f <- reg.pattern.list[[pattern]]
    yname <- paste0("y_",pattern)
    reg.dt[, (yname) := f(x,person)+rnorm(N, sd=0.5)][]
    task.dt <- reg.dt[, c("x","person",yname), with=FALSE]
    task.obj <- mlr3::TaskRegr$new(
      pattern, task.dt, target=yname)
    task.obj$col_roles$stratum <- "person"
    task.obj$col_roles$subset <- "person"
    reg.task.list[[pattern]] <- task.obj
  }
  reg.learner.list <- list(
    mlr3::LearnerRegrFeatureless$new())
  if(requireNamespace("rpart")){
    reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
  }
  (bench.grid <- mlr3::benchmark_grid(
    reg.task.list,
    reg.learner.list,
    SOAK))
  bench.result <- mlr3::benchmark(bench.grid)
  bench.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
  if(interactive())plot(bench.score)
  set(bench.score, j="foo", value=1)
  expect_is(bench.score, "score")
  expect_identical(last_lev(bench.score$Train_subsets), "other")
  bench.pvalue <- mlr3resampling::pvalue(bench.score)
  if(interactive())plot(bench.pvalue)
  expect_identical(last_lev(bench.pvalue$stats$Train_subsets), "other")
})

test_that("plot ok without other", {
  N <- 80
  set.seed(1)
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=rep(1:2, each=0.5*N))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^person)
  SAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  SAK$param_set$values$subsets <- "SA"
  reg.task.list <- list()
  for(pattern in names(reg.pattern.list)){
    f <- reg.pattern.list[[pattern]]
    yname <- paste0("y_",pattern)
    reg.dt[, (yname) := f(x,person)+rnorm(N, sd=0.5)][]
    task.dt <- reg.dt[, c("x","person",yname), with=FALSE]
    task.obj <- mlr3::TaskRegr$new(
      pattern, task.dt, target=yname)
    task.obj$col_roles$stratum <- "person"
    task.obj$col_roles$subset <- "person"
    reg.task.list[[pattern]] <- task.obj
  }
  reg.learner.list <- list(
    mlr3::LearnerRegrFeatureless$new())
  if(requireNamespace("rpart")){
    reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
  }
  (bench.grid <- mlr3::benchmark_grid(
    reg.task.list,
    reg.learner.list,
    SAK))
  bench.result <- mlr3::benchmark(bench.grid)
  bench.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
  if(interactive())plot(bench.score)
  expect_identical(last_lev(bench.score$Train_subsets), "same")
  bench.pvalue <- mlr3resampling::pvalue(bench.score)
  expect_identical(bench.pvalue$stats$value_length, rep(3L, 16))
  expect_identical(as.character(bench.pvalue$pvalues$Train_subsets), rep("all-same", 8))
  if(interactive())plot(bench.pvalue)
  expect_identical(last_lev(bench.pvalue$stats$Train_subsets), "same")
})

test_that("pvalue ok with sizes=0", {
  N <- 80
  set.seed(1)
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=rep(1:2, each=0.5*N))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^person)
  SAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  SAK$param_set$values$subsets <- "SA"
  SAK$param_set$values$sizes <- 0
  reg.task.list <- list()
  for(pattern in names(reg.pattern.list)){
    f <- reg.pattern.list[[pattern]]
    yname <- paste0("y_",pattern)
    reg.dt[, (yname) := f(x,person)+rnorm(N, sd=0.5)][]
    task.dt <- reg.dt[, c("x","person",yname), with=FALSE]
    task.obj <- mlr3::TaskRegr$new(
      pattern, task.dt, target=yname)
    task.obj$col_roles$stratum <- "person"
    task.obj$col_roles$subset <- "person"
    reg.task.list[[pattern]] <- task.obj
  }
  reg.learner.list <- list(
    mlr3::LearnerRegrFeatureless$new())
  if(requireNamespace("rpart")){
    reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
  }
  (bench.grid <- mlr3::benchmark_grid(
    reg.task.list,
    reg.learner.list,
    SAK))
  bench.result <- mlr3::benchmark(bench.grid)
  bench.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
  if(interactive())plot(bench.score)
  expect_identical(last_lev(bench.score$Train_subsets), "same")
  expect_silent({
    bench.pvalue <- mlr3resampling::pvalue(bench.score)
  })
  expect_identical(bench.pvalue$stats$value_length, rep(3L, 16))
  expect_identical(as.character(bench.pvalue$pvalues$Train_subsets), rep("all-same", 8))
  if(interactive())plot(bench.pvalue)
  expect_identical(last_lev(bench.pvalue$stats$Train_subsets), "same")
})

test_that("instantiate ok for large task", {
  df <- data.frame(x=1:1e5)
  big_task <- mlr3::TaskRegr$new("big", df, "x")
  SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  expect_silent({
    SOAK$instantiate(big_task)
  })
})

test_that("fold role works for reproducibility", {
  task_list <- mlr3::tsks(c("spam", "german_credit"))
  tasks_with_fold <- list()
  for(task_i in seq_along(task_list)){
    task <- task_list[[task_i]]
    task_dt <- task$data()
    task_dt[, Fold := rep(1:3, length.out=.N), by=c(task$col_roles$target)]
    ftask <- mlr3::TaskClassif$new(
      task_dt, id=task$id, target=task$col_roles$target)
    ftask$col_roles$feature <- task$col_roles$feature
    ftask$col_roles$fold <- "Fold"
    tasks_with_fold[[task$id]] <- ftask
  }
  learner_list <- list(
    mlr3::LearnerClassifFeatureless$new())
  if(requireNamespace("rpart")){
    learner_list$rpart <- mlr3::LearnerClassifRpart$new()
  }
  for(learner_i in seq_along(learner_list)){
    L <- learner_list[[learner_i]]
    L$predict_type <- "prob"
  }
  measure_list <- mlr3::msrs(c("classif.auc","classif.acc"))
  (kfoldcv <- mlr3resampling::ResamplingSameOtherSizesCV$new())
  (bgrid <- mlr3::benchmark_grid(tasks_with_fold, learner_list, kfoldcv))
  proj_dir <- if(interactive())"~/testproj" else tempfile()
  unlink(proj_dir, recursive = TRUE)
  mlr3resampling::proj_grid(
    proj_dir, tasks_with_fold, learner_list, kfoldcv,
    score_args = measure_list)
  bench_result <- mlr3::benchmark(bgrid)
  out.list <- list(
    bench=bench_result$score(measure_list),
    proj=mlr3resampling::proj_compute_all(proj_dir))
  check <- sapply(out.list, function(dt)data.table(dt)[
  , .(task_id, learner_id, iteration, classif.auc, classif.acc)]
  , simplify=FALSE)
  with(check, expect_identical(proj, bench))
  expect_error({
    mlr3resampling::proj_test(proj_dir)
  }, "Detected zero count for at least one class label after train/test split, which is invalid for a classification task; typically this can be fixed by setting task$col_roles$stratum", fixed=TRUE)
})

test_that("fold and stratum roles work for reproducibility", {
  task_list <- mlr3::tsks(c("spam", "german_credit"))
  tasks_with_fold <- list()
  for(task_i in seq_along(task_list)){
    task <- task_list[[task_i]]
    task_dt <- task$data()
    task_dt[, Fold := rep(1:3, length.out=.N), by=c(task$col_roles$target)]
    ftask <- mlr3::TaskClassif$new(
      task_dt, id=task$id, target=task$col_roles$target)
    ftask$col_roles$feature <- task$col_roles$feature
    ftask$col_roles$fold <- "Fold"
    ftask$col_roles$stratum <- c("Fold", task$col_roles$target)
    tasks_with_fold[[task$id]] <- ftask
  }
  learner_list <- list(
    mlr3::LearnerClassifFeatureless$new())
  if(requireNamespace("rpart")){
    learner_list$rpart <- mlr3::LearnerClassifRpart$new()
  }
  for(learner_i in seq_along(learner_list)){
    L <- learner_list[[learner_i]]
    L$predict_type <- "prob"
  }
  measure_list <- mlr3::msrs(c("classif.auc","classif.acc"))
  (kfoldcv <- mlr3resampling::ResamplingSameOtherSizesCV$new())
  (bgrid <- mlr3::benchmark_grid(tasks_with_fold, learner_list, kfoldcv))
  proj_dir <- if(interactive())"~/testproj" else tempfile()
  unlink(proj_dir, recursive = TRUE)
  mlr3resampling::proj_grid(
    proj_dir, tasks_with_fold, learner_list, kfoldcv,
    score_args = measure_list)
  bench_result <- mlr3::benchmark(bgrid)
  out.list <- list(
    bench=bench_result$score(measure_list),
    proj=mlr3resampling::proj_compute_all(proj_dir))
  check <- sapply(out.list, function(dt)data.table(dt)[
  , .(task_id, learner_id, iteration, classif.auc, classif.acc)]
  , simplify=FALSE)
  with(check, expect_identical(proj, bench))
  tlist <- mlr3resampling::proj_test(proj_dir)
  expect_identical(names(tlist), c("grid_jobs.csv", "results.csv"))
})

test_that("fold role is checked", {
  soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  soak$param_set$values$folds <- 2L
  group.dt <- data.table(
    x=1:6,
    y=factor(rep(c("a", "b"), each=3)),
    grp=c(1, 1, 2, 3, 3, 4),
    Fold=c(1, 2, 1, 1, 2, 2))
  group.task <- mlr3::TaskClassif$new("group_fold_bad", group.dt, target="y")
  group.task$col_roles$feature <- "x"
  group.task$col_roles$group <- "grp"
  group.task$col_roles$fold <- "Fold"
  group.task$col_roles$stratum <- "y"
  expect_error({
    soak$instantiate(group.task)
  }, "task$col_roles$fold must be constant within each group", fixed=TRUE)
})

if(requireNamespace("mlr3learners"))test_that("cv.glmnet same result between two tests", {
  spam <- mlr3::tsk("spam")
  spam$col_roles$stratum <- "type"
  sdata <- data.table(spam$data(), Fold=rep(1:3, length.out = spam$nrow))
  spam_with_fold <- mlr3::TaskClassif$new(
    "spam_with_fold", sdata, target="type")
  spam_with_fold$col_roles$stratum <- c("type","Fold")
  spam_with_fold$col_roles$fold <- "Fold"
  spam_with_fold$col_roles$feature <- spam$col_roles$feature
  L <- list(
    mlr3learners::LearnerClassifCVGlmnet$new(),
    mlr3::LearnerClassifRpart$new())
  for(learner.i in seq_along(L)){
    L[[learner.i]]$predict_type <- "prob"
  }
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  pdir <- if(interactive())"~/pdir" else tempfile()
  task_list <- list(spam, spam_with_fold)
  ## test 1 default set train seed.
  unlink(pdir, recursive = TRUE)
  mlr3resampling::proj_grid(pdir, task_list, L, kfold, score_args=mlr3::msrs("classif.auc"))
  set.seed(1)#needed to avoid spurious err in checks.
  test_res_list <- list()
  for(run.num in 1:2){
    tres <- mlr3resampling::proj_test(pdir, min_samples_per_stratum = 20)
    test_res_list[[run.num]] <- data.table(
      run=paste0("run", run.num), tres$results.csv)
  }
  test_res <- rbindlist(test_res_list)[
  , algorithm := sub("classif.", "", learner_id)]
  test_wide <- dcast(
    test_res, task_id + algorithm ~ run, value.var="classif.auc")
  test_wide[, expect_identical(run1, run2)]
  ## test 2 train_seed=NA means do not set seed.
  unlink(pdir, recursive = TRUE)
  mlr3resampling::proj_grid(pdir, task_list, L, kfold, score_args=mlr3::msrs("classif.auc"), train_seed=NA_integer_)
  set.seed(1)#needed to avoid spurious err in checks.
  test_res_list <- list()
  for(run.num in 1:2){
    tres <- mlr3resampling::proj_test(pdir, min_samples_per_stratum = 20)
    test_res_list[[run.num]] <- data.table(
      run=paste0("run", run.num), tres$results.csv)
  }
  test_res <- rbindlist(test_res_list)[
  , algorithm := sub("classif.", "", learner_id)]
  test_wide <- dcast(
    test_res, task_id + algorithm ~ run, value.var="classif.auc")
  test_wide[, expect_identical(run1, run2)]
})

test_that("random folds for irregular sized groups with multiple strata", {
  ## https://github.com/tdhock/mlr3resampling/issues/86
  set.seed(123)
  n = 20
  files <- data.table(
    PID = sample(1:10, n, replace = TRUE),   # ID for Grouping
    y = factor(ifelse(rbinom(n, size = 1, prob = 0.2) == 1, "A", "B")),
    feature1 = rnorm(n, mean = 0, sd = 1),
    feature2 = rnorm(n, mean = 5, sd = 2),
    feature3 = rnorm(n, mean = 100, sd = 15))
  files[, table(PID, y)]
  comb.dt <- CJ(set_group=c(TRUE,FALSE), run=paste0("run", 1:2))
  fpg.dt.list <- list()
  for(comb.i in 1:nrow(comb.dt)){
    comb <- comb.dt[comb.i]
    task_amr = mlr3::as_task_classif(files, target = "y")
    task_amr$positive = "A"
    if(comb$set_group)task_amr$set_col_roles("PID", roles = "group")
    task_amr$set_col_roles("y", roles = c("target", "stratum"))
    task_amr$col_roles
    test.validation.resampling = mlr3resampling::ResamplingSameOtherSizesCV$new()
    test.validation.resampling$instantiate(task_amr)
    fold.dt <- test.validation.resampling$instance$fold.dt
    fold.dt[, table(fold, stratum)]
    fold.dt[, table(group, stratum)]
    fpg <- fold.dt[, .(N=.N), by=.(fold, group)]
    fpg.dt.list[[comb.i]] <- data.table(comb, fpg)
  }
  fpg.dt <- rbindlist(fpg.dt.list)
  folds <- dcast(fpg.dt, set_group + group ~ run, value.var="fold")
  expect_false(folds[set_group==FALSE, identical(run1, run2)])
  expect_false(folds[set_group==TRUE,  identical(run1, run2)])
})

test_that("prop sizes for regular sized groups with multiple strata", {
  ## https://github.com/tdhock/mlr3resampling/issues/86
  files <- data.table(g=1:75)[
  , if(g<=5)data.table(y=c("rare","rare"))
    else if(g<=10)data.table(y=c("rare","common"))
    else data.table(y=c("common","common"))
  , by=g]
  expect_error({
    files[, mlr3resampling:::stratified_group_cv_Wasikowski_interface(as.integer(factor(y))-1L, g, 2)]
  }, "need at least one of each group from zero to max")
  expect_error({
    files[, mlr3resampling:::stratified_group_cv_Wasikowski_interface(as.integer(factor(y)), g-1L, 2)]
  }, "need at least one of each stratum from zero to max")
  fold.vec <- files[, mlr3resampling:::stratified_group_cv_Wasikowski_interface(as.integer(factor(y))-1L, g-1L, 2)]
  fold.tab <- table(fold.vec)
  expect_identical(names(fold.tab), c("0","1"))
  files[, table(g, y)]
  files[, table(y)]
  comb.dt <- CJ(set_group=c(TRUE,FALSE), nfold=c(3,5))
  fpg.dt.list <- list()
  set.seed(1)
  grdt <- list()
  for(algo in c("RSS", "Wasikowski", "WasikowskiLimitedMemory")){
    for(comb.i in 1:nrow(comb.dt)){
      comb <- comb.dt[comb.i]
      task_amr = mlr3::as_task_classif(files, target = "y")
      if(comb$set_group)task_amr$set_col_roles("g", roles = "group")
      task_amr$set_col_roles("y", roles = c("target", "stratum"))
      test.validation.resampling = mlr3resampling::ResamplingSameOtherSizesCV$new()
      test.validation.resampling$param_set$values$folds <- comb$nfold
      test.validation.resampling$param_set$values$group_stratum_algo <- algo
      test.validation.resampling$instantiate(task_amr)
      fold.dt <- test.validation.resampling$instance$fold.dt
      grdt[[comb.i]] <- fold.dt
      fold.dt[, table(fold, stratum)]
      fold.dt[, table(group, stratum)]
      fpg <- fold.dt[, .(N=.N), by=.(fold, stratum)]
      fpg.dt.list[[paste(algo, comb.i)]] <- data.table(algo, comb, fpg)
    }
  }
  ## calculations to see why there are not same strata counts across folds.
  ##dcast(melt(dcast(grdt[[3]][, .(row=.I, fold, y, g_ord)][, r := min(row), by=g_ord], r ~ fold+y), id="r")[, cum := cumsum(value), by=variable], r ~ variable, value.var="cum")
  ## 69:   137       45      1       45      1       44      2
  ## 70:   139       46      2       45      1       44      2
  ideal <- c(45,5)
  ideal-c(45,1)#16
  ideal-c(46,2)#10
  ideal-c(44,2)#10
  ideal-c(45,3)#4
  fpg.dt <- rbindlist(fpg.dt.list)
  expect_equal(fpg.dt[algo=="RSS" & nfold==3, sort(N)], rep(c(5,45),each=6))
  expect_equal(fpg.dt[algo=="RSS" & nfold==5, sort(N)], rep(c(3,27),each=10))
})

gdt <- function(..., strata=2){
  m <- matrix(c(...), ncol=strata, byrow=TRUE)
  data.table(row.i=1:nrow(m))[, {
    times <- m[row.i,]
    y <- rep(1:strata, times)
    p <- paste(times, collapse=",")
    g <- paste0(row.i, "_", p)
    data.table(g, y)
  }, by=row.i]
}

test_that("deterministic optimal fold assignment for unique group sizes with multiple strata", {
  files <- gdt(
    1,1,
    0,2,
    1,2,
    0,1,
    1,3)
  ideal <- as.numeric(files[, table(y)]/3)
  files[, table(g, y)]
  comb.dt <- CJ(set_group=c(TRUE,FALSE), run=paste0("run", 1:2))
  fpg.dt.list <- list()
  for(comb.i in 1:nrow(comb.dt)){
    comb <- comb.dt[comb.i]
    task_amr = mlr3::as_task_classif(files, target = "y")
    if(comb$set_group)task_amr$set_col_roles("g", roles = "group")
    task_amr$set_col_roles("y", roles = c("target", "stratum"))
    task_amr$col_roles
    test.validation.resampling = mlr3resampling::ResamplingSameOtherSizesCV$new()
    test.validation.resampling$param_set$values$group_stratum_algo <- "RSS"
    test.validation.resampling$instantiate(task_amr)
    fold.dt <- test.validation.resampling$instance$fold.dt
    fold.dt[, table(y, fold)]
    expect_equal(sum(fold.dt[, t(table(fold, stratum))]==ideal), 6)
    fpg <- fold.dt[, .(N=.N), by=.(fold, group)]
    fpg.dt.list[[comb.i]] <- data.table(comb, fpg)
  }
  fpg.dt <- rbindlist(fpg.dt.list)
  folds <- dcast(fpg.dt, set_group + group ~ run, value.var="fold")
  expect_false(folds[set_group==FALSE, identical(run1, run2)])
  ## the groups are all different, so different variance, same sort
  ## order, no randomness between runs.
  folds[set_group==TRUE,  expect_identical(run1, run2)]
})

test_that("compare assignment for five unique group sizes with multiple strata", {
  files <- gdt(
    1,1,
    0,2,
    1,2,
    0,1,
    1,3)
  ideal <- as.numeric(files[, table(y)]/3)
  files[, table(g, y)]
  tab.list <- list()
  for(group_stratum_algo in c("Wasikowski","RSS")){
    set.seed(1)
    task_amr = mlr3::as_task_classif(files, target = "y")
    task_amr$set_col_roles("g", roles = "group")
    task_amr$set_col_roles("y", roles = c("target", "stratum"))
    test.validation.resampling = mlr3resampling::ResamplingSameOtherSizesCV$new()
    test.validation.resampling$param_set$values$group_stratum_algo <- group_stratum_algo
    test.validation.resampling$instantiate(task_amr)
    fold.dt <- test.validation.resampling$instance$fold.dt[
    , var := var(table(y))
    , by=group][]
    group.dt <- fold.dt[, .SD[1], by=group][, counts := sub(".*_", "", group)]
    print(if("rss" %in% names(fold.dt))group.dt[
      order(rss), .(counts, fold, rss)
    ] else group.dt[
      order(neg_sd, g_ord),  .(counts, fold, var=neg_sd^2, g_ord)])
    print(tab.list[[group_stratum_algo]] <- fold.dt[, table(y, fold)])
  }
  expect_equal(sort(tab.list$Wasikowski), c(1,1,1,2,3,4))
  expect_equal(sort(tab.list$RSS), c(1,1,1,3,3,3))
})

test_that("deterministic fold assignment for ties ideal 6 5", {
  files <- gdt(
    5,3,
    3,0,
    1,2,
    1,2,
    1,2,
    1,1)
  files[, table(y)]
  task_amr = mlr3::as_task_classif(files, target = "y")
  task_amr$set_col_roles("g", roles = "group")
  task_amr$set_col_roles("y", roles = c("target", "stratum"))
  test.validation.resampling = mlr3resampling::ResamplingSameOtherSizesCV$new()
  test.validation.resampling$param_set$values$folds <- 2
  test.validation.resampling$instantiate(task_amr)
  fold.dt <- test.validation.resampling$instance$fold.dt
  fold.dt[
  , sd := sd(table(y))
  , by=group][
  , .SD[1]
  , by=group][, .(
    counts=sub(".*_", "", group),
    sd, rss, nrow=-neg_nrow, freq=-neg_freq)]
  stab <- sort(fold.dt[, table(stratum, fold)])
  expect_equal(stab, c(5,5,6,6))
  for(fsign in c(-1, 1)){
    fold.ord <- fold.dt[order(rss, neg_nrow, fsign*neg_freq)][, let(
      g.int = as.integer(factor(group, unique(group))),
      Stratum = paste0("s", stratum)
    )][]
    fold.out <- fold.ord[, mlr3resampling:::stratified_group_cv_RSS_interface(stratum-1, g.int, 2L)+1L]
    fwide <- dcast(data.table(fold.ord, fold.out), g.int + fold.out ~ Stratum, length)
    for(f in 1:2){
      fwide[
      , paste0("f", f, "s", 1:2) := lapply(.SD, function(x)cumsum(ifelse(fold.out==f, x, 0)))
      , .SDcols=paste0("s", 1:2)][]
    }
    p <- function(...)apply(cbind(...), 1, paste, collapse=",")
    print(fwide[, .(
      group=p(s1,s2),
      fold=fold.out,
      fold1=p(f1s1,f1s2),
      fold2=p(f2s1,f2s2)
    )])
    print(table(fold.ord$stratum, fold.out))
  }
})

test_that("folds ok for AZtrees(group,stratum)", {
  data(AZtrees, package="mlr3resampling")
  trees_task <- mlr3::as_task_classif(AZtrees, target="y")
  trees_task$col_roles$group <- "polygon"
  trees_task$col_roles$stratum <- "y"
  cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  folds <- 2
  cv$param_set$values$folds <- folds
  cv$instantiate(trees_task)
  glist <- list(
    rows=cv$instance$fold.dt,
    groups=cv$instance$fold.dt[, .(rows=.N), by=.(group, stratum, fold)])
  olist <- list()
  for(item in names(glist)){
    idt <- glist[[item]]
    olist[[item]] <- idt[, table(stratum, fold)]
  }
  olist
  computed.mean <- with(olist, mean(groups==groups[,1]))
  expect_lt(computed.mean, 1)
  rows.per.fold <- as.numeric(table(AZtrees$y))/folds
  expect_equal(sum(olist$rows==rows.per.fold), folds*length(rows.per.fold))
})

test_that("folds ok for AZtrees(group)", {
  data(AZtrees, package="mlr3resampling")
  trees_task <- mlr3::as_task_classif(AZtrees, target="y")
  trees_task$col_roles$group <- "polygon"
  cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  folds <- 4
  cv$param_set$values$folds <- folds
  cv$instantiate(trees_task)
  glist <- list(
    rows=cv$instance$fold.dt,
    groups=cv$instance$fold.dt[, .(rows=.N), by=.(group, stratum, fold)])
  olist <- list()
  for(item in names(glist)){
    idt <- glist[[item]]
    olist[[item]] <- idt[, table(stratum, fold)]
  }
  olist
  computed.mean <- with(olist, mean(groups==groups[1]))
  expect_lt(computed.mean, 1)
  rows.per.fold <- nrow(AZtrees)/folds
  expect_equal(sum(olist$rows==rows.per.fold), folds)
})

test_that("folds ok for AZtrees(stratum)", {
  data(AZtrees, package="mlr3resampling")
  trees_task <- mlr3::as_task_classif(AZtrees, target="y")
  trees_task$col_roles$stratum <- "y"
  cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  folds <- 2
  cv$param_set$values$folds <- folds
  cv$instantiate(trees_task)
  glist <- list(
    rows=cv$instance$fold.dt,
    groups=cv$instance$fold.dt[, .(rows=.N), by=.(group, stratum, fold)])
  olist <- list()
  for(item in names(glist)){
    idt <- glist[[item]]
    olist[[item]] <- idt[, table(stratum, fold)]
  }
  rows.per.fold <- as.numeric(table(AZtrees$y))/folds
  expect_equal(sum(olist$rows==rows.per.fold), folds*length(rows.per.fold))
  expect_equal(sum(olist$groups==rows.per.fold), folds*length(rows.per.fold))
})

test_that("error for fold length 2",{
  spam <- mlr3::tsk("spam")
  spam$col_roles$stratum <- "type"
  sdata <- data.table(
    spam$data(),
    myfold=rep(1:3, length.out = spam$nrow),
    Fold=rep(1:3, length.out = spam$nrow))
  spam_with_fold <- mlr3::TaskClassif$new(
    "spam_with_fold", sdata, target="type")
  spam_with_fold$col_roles$fold <- c("Fold","myfold")
  spam_with_fold$col_roles$feature <- spam$col_roles$feature
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  expect_error({
    kfold$instantiate(spam_with_fold)
  }, "fold role must have length 0 or 1")
  expect_null(kfold$instance)
  spam_with_fold$col_roles$fold <- "Fold"
  kfold$instantiate(spam_with_fold)
  expect_is(kfold$instance, "list")
})

test_that("ResamplingSameOtherSizesCV error for 2 subset vars", {
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$subset <- c("random_group", "agroup")
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  expect_error({
    kfold$instantiate(reg.task)
  }, "subset role must be length 0 or 1")
  expect_null(kfold$instance)
  reg.task$col_roles$subset <- "random_group"
  kfold$instantiate(reg.task)
  expect_is(kfold$instance, "list")
})

test_that("plot method works with ResamplingCV", {
  spam <- mlr3::tsk("spam")
  fless <- mlr3::lrn("classif.featureless")
  fless$predict_type <- "prob"
  cv <- mlr3::rsmp("cv", folds=2)
  bgrid <- mlr3::benchmark_grid(spam, fless, cv)
  bres <- mlr3::benchmark(bgrid)
  score_ob = mlr3resampling::score(bres, mlr3::msrs('classif.auc'))
  gg <- plot(score_ob)
  expect_is(gg, "ggplot")
  print(gg)
})
