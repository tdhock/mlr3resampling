library(testthat)
library(data.table)
if(requireNamespace("lgr"))lgr::get_logger("mlr3")$set_threshold("warn")

test_that("resampling error if no group", {
  itask <- mlr3::TaskClassif$new("iris", iris, target="Species")
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_error({
    same_other$instantiate(itask)
  }, 'task has no subset, but at least one subset variable is required', fixed=TRUE)
})

test_that("resampling error if no strata", {
  iris.dt <- data.table(iris)[, g := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "g"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_error({
    same_other$instantiate(itask)
  }, 'task has no strata, but at least one stratum variable is required; at least assign the subset variable to a stratum', fixed=TRUE)
})

test_that("instantiation creates instance", {
  iris.dt <- data.table(iris)[, g := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "g"
  itask$col_roles$stratum <- "g"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  same_other$instantiate(itask)
  expect_identical(same_other$instance$id.dt$g, iris.dt$g)
})

test_that("error for subset named subset", {
  iris.dt <- data.table(iris)[, subset := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "subset"
  itask$col_roles$stratum <- "subset"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role subset must not be named subset; please fix by renaming subset col")
})

test_that("error for group named row_id", {
  iris.dt <- data.table(iris)[, row_id := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "row_id"
  itask$col_roles$stratum <- "row_id"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role subset must not be named row_id; please fix by renaming row_id col")
})

test_that("error for group named fold", {
  iris.dt <- data.table(iris)[, fold := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "fold"
  itask$col_roles$stratum <- "fold"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role subset must not be named fold; please fix by renaming fold col")
})

test_that("error for group named display_row", {
  iris.dt <- data.table(iris)[, display_row := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "display_row"
  itask$col_roles$stratum <- "display_row"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role subset must not be named display_row; please fix by renaming display_row col")
})

test_that("error for group named test", {
  iris.dt <- data.table(iris)[, test := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$col_roles$subset <- "test"
  itask$col_roles$stratum <- "test"
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role subset must not be named test; please fix by renaming test col")
})

test_that("errors and result for 10 train data in small stratum", {
  size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
  size_cv$param_set$values$folds <- 2
  i10.dt <- data.table(iris)[1:70]
  i10.task <- mlr3::TaskClassif$new(
    "i10", i10.dt, target="Species"
  )$set_col_roles("Species",c("target","stratum"))
  expect_error({
    size_cv$instantiate(i10.task)
  },
  "max_train_data=10 (in smallest stratum) but should be larger than min_train_data=10, please fix by decreasing min_train_data",
  fixed=TRUE)
  size_cv$param_set$values$min_train_data <- 9
  expect_error({
    size_cv$instantiate(i10.task)
  },
  "train sizes not unique, please decrease train_sizes",
  fixed=TRUE)
  size_cv$param_set$values$train_sizes <- 2
  size_cv$instantiate(i10.task)
  size.tab <- table(size_cv$instance$iteration.dt[["small_stratum_size"]])
  expect_identical(names(size.tab), c("9","10"))
})

test_that("strata respected in all sizes", {
  size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
  size_cv$param_set$values$min_train_data <- 5
  size_cv$param_set$values$folds <- 5
  N <- 100
  imbalance <- 4
  strat.vec <- ifelse((1:imbalance)<imbalance, "A","B")
  istrat.dt <- data.table(iris[1:N,], strat=factor(rep(strat.vec, l=N)))
  smallest.size.tab <- table(
    istrat.dt[["strat"]]
  )/N*imbalance*size_cv$param_set$values$min_train_data
  istrat.task <- mlr3::TaskClassif$new(
    "istrat", istrat.dt, target="Species"
  )$set_col_roles("strat", "stratum")
  size_cv$instantiate(istrat.task)
  min.dt <- size_cv$instance$iteration.dt[train_size==min(train_size)]
  for(min.i in 1:nrow(min.dt)){
    min.row <- min.dt[min.i]
    train.i <- min.row$train[[1]]
    strat.tab <- table(istrat.dt[train.i, strat])
    expect_equal(strat.tab, smallest.size.tab)
  }
})

test_that("train set max size 67 for 100 data", {
  size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
  i100.dt <- data.table(iris)[1:100]
  i100.task <- mlr3::TaskClassif$new("i10", i100.dt, target="Species")
  size_cv$instantiate(i100.task)
  inst <- size_cv$instance
  computed.counts <- inst$id.dt[, .(rows=.N), keyby=fold]
  expected.counts <- data.table(
    fold=1:3,
    rows=as.integer(c(34,33,33)),
    key="fold")
  expect_equal(computed.counts, expected.counts)
  l.train <- sapply(inst$iteration.dt$train, length)
  expect_equal(l.train, inst$iteration.dt$train_size)
  expect_equal(max(l.train), 67)
})

test_that("test fold 1 for iteration 1", {
  set.seed(1)
  size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
  i100.dt <- data.table(iris)[1:100]
  i100.task <- mlr3::TaskClassif$new("i10", i100.dt, target="Species")
  size_cv$instantiate(i100.task)
  inst <- size_cv$instance
  expect_equal(inst$iteration.dt$test.fold[1], 1)
})

## ResamplingSameOtherSizesCV
N <- 2100
abs.x <- 20
set.seed(1)
x.vec <- sort(runif(N, -abs.x, abs.x))
(task.dt <- data.table(
  x=x.vec,
  y = sin(x.vec)+rnorm(N,sd=0.5)))
atomic.group.size <- 2
task.dt[, agroup := rep(seq(1, N/atomic.group.size), each=atomic.group.size)][]
task.dt[, random_group := rep(
  rep(c("A","B","B","C","C","C","C"), each=atomic.group.size),
  l=.N
)][]
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
  n.folds <- 3
  same_other_sizes_cv$param_set$values$folds <- n.folds
  same_other_sizes_cv$param_set$values$seeds <- 1
  same_other_sizes_cv$param_set$values$ratio <- 0.5
  same_other_sizes_cv$param_set$values$sizes <- 0
  same_other_sizes_cv$param_set$values$ignore_subset <- FALSE
  same_other_sizes_cv$instantiate(reg.task)
  computed <- same_other_sizes_cv$instance$iteration.dt
  n.train.per.test <- 6
  expect_equal(nrow(computed), n.folds*n.subsets*n.train.per.test)
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
      test.subset=1,
      algorithm="featureless",
      test.fold=c(1,2,1,2,1,2),
      regr.rmse=c(24.1,25,26.2,27,28.3,29)+100,
      train.subsets=c("same","same","other","other","all","all")),
    data.table(
      task_id="easy",
      test.subset=1,
      algorithm="rpart",
      test.fold=c(1,2,1,2,1,2),
      regr.rmse=c(8.1,9,12.2,13,22.3,23),
      train.subsets=c("all","all","other","other","same","same")))
  bench.plist <- mlr3resampling::pvalue(bench.score)
  expect_equal(bench.plist$pvalues[algorithm=="featureless", hjust], c(1,1))
  expect_equal(bench.plist$pvalues[algorithm=="rpart", hjust], c(0,0))
  expect_equal(bench.plist$stats[algorithm=="featureless", hjust], c(1,1,1))
  expect_equal(bench.plist$stats[algorithm=="rpart", hjust], c(0,0,0))
})

test_that("hjust=0.5 for algo in middle", {
  score_dt <- rbind(
    data.table(
      task_id="test",
      test.subset="foo",
      train.subsets="same",
      test.fold=1:3,
      algorithm="featureless",
      classif.auc=0.5),
    data.table(
      task_id="test",
      test.subset="foo",
      train.subsets="same",
      test.fold=1:3,
      algorithm="conv",
      classif.auc=c(0.91,0.915, 0.93)),
    data.table(
      task_id="test",
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
  mlr3resampling::proj_compute_all(pkg.proj.dir)
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
  }, "no score_args nor save_pred, so there will no test error results")
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

mlr3torch_available <- requireNamespace("mlr3torch") && torch::torch_is_installed()
if(mlr3torch_available)test_that("mlr3torch history saved", {
  N <- 80
  set.seed(1)
  people <- c("Alice","Bob")
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(people, each=0.5*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  kfold$param_set$values$folds <- 2
  subsets <- "SA"
  kfold$param_set$values$subsets <- subsets
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
  Tlrn <- mlr3torch::LearnerTorchMLP$new(task_type="regr")
  mlr3::set_validate(Tlrn, validate = 0.5)
  n.epochs <- 3
  Tlrn$callbacks <- mlr3torch::t_clbk("history")
  Tlrn$param_set$values$patience <- n.epochs
  Tlrn$param_set$values$batch_size <- 10
  Tlrn$param_set$values$epochs <- paradox::to_tune(upper = n.epochs, internal = TRUE)
  Tlrn$param_set$values[c("measures_train","measures_valid")] <- mlr3::msrs(c("regr.rmse"))
  reg.learner.list <- list(
    mlr3tuning::auto_tuner(
      learner = Tlrn,
      tuner = mlr3tuning::tnr("internal"),
      resampling = mlr3::rsmp("insample"),
      measure = mlr3::msr("internal_valid_score", minimize = TRUE),
      term_evals = 1,
      id="torch_linear",
      store_models = TRUE),
    mlr3::LearnerRegrFeatureless$new())
  pkg.proj.dir <- tempfile()
  get_history <- function(L){
    if(grepl("torch", L$id)){
      V <- L$tuning_result$internal_tuned_values[[1]]
      M <- L$archive$learners(1)[[1]]$model
      M$callbacks$history
    }
  }
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")),
    save_learner = get_history)
  grid_jobs <- fread(file.path(pkg.proj.dir, "grid_jobs.csv"))
  expect_equal(grid_jobs[test.fold==1 & test.subset=="Bob" & train.subsets=="all" & task_id=="easy", unique(learner_id)], c("torch_linear", "regr.featureless"))
  expect_equal(grid_jobs[learner.i==1, unique(n.train.groups)], c(40, 20))
  expect_true(all(grid_jobs$status=="not started"))
  expected_base <- length(reg.task.list)*kfold$param_set$values$folds*length(people)*nchar(subsets)
  expected_epochs <- n.epochs*expected_base
  expected_jobs <- length(reg.learner.list)*expected_base
  expect_equal(nrow(grid_jobs), expected_jobs)
  results.csv <- file.path(pkg.proj.dir, "results.csv")
  expect_false(file.exists(results.csv))
  row1 <- mlr3resampling::proj_compute(1, pkg.proj.dir)
  expected_learner_cols <- c("epoch","train.regr.rmse","valid.regr.rmse")
  expect_train_valid <- function(R){
    train_valid_rows <- R$learner[[1]]
    expect_equal(nrow(train_valid_rows), n.epochs)
    expect_identical(names(train_valid_rows), expected_learner_cols)
  }
  expect_train_valid(row1)
  from.disk <- mlr3resampling::proj_results(pkg.proj.dir)
  expect_train_valid(from.disk)
  computed <- mlr3resampling::proj_compute_all(pkg.proj.dir)
  model_dt <- fread(file.path(pkg.proj.dir, "learners.csv"))
  expected_csv_cols <- c("grid_job_i", expected_learner_cols)
  expect_identical(names(model_dt), expected_csv_cols)
  expect_equal(nrow(model_dt), expected_epochs)
  results_dt <- fread(file.path(pkg.proj.dir, "results.csv"))
  pval_list <- mlr3resampling::pvalue(results_dt)
  expect_is(pval_list, "pvalue")
  expect_true(all(pval_list$pvalues$Train_subsets=="all-same"))
  if(interactive())plot(pval_list)
  csv_data_list <- mlr3resampling::proj_fread(pkg.proj.dir)
  expected_join_cols <- c(
    expected_csv_cols,
    "task_id", "learner_id", "resampling_id", "test.subset", "train.subsets",
    "groups", "test.fold", "seed", "n.train.groups", "iteration")
  expect_identical(names(csv_data_list$learners.csv), expected_join_cols)
})

if(mlr3torch_available)test_that("mlr3torch history and weights saved", {
  N <- 80
  set.seed(1)
  people <- c("Alice","Bob","Bob","Bob")
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(people, each=0.25*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  kfold$param_set$values$folds <- 2
  kfold$param_set$values$sizes <- 1
  subsets <- "S"
  kfold$param_set$values$subsets <- subsets
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
  Tlrn <- mlr3torch::LearnerTorchMLP$new(task_type="regr")
  mlr3::set_validate(Tlrn, validate = 0.5)
  n.epochs <- 3
  Tlrn$callbacks <- mlr3torch::t_clbk("history")
  Tlrn$param_set$values$patience <- n.epochs
  Tlrn$param_set$values$batch_size <- 10
  Tlrn$param_set$values$epochs <- paradox::to_tune(upper = n.epochs, internal = TRUE)
  Tlrn$param_set$values[c("measures_train","measures_valid")] <- mlr3::msrs(c("regr.rmse"))
  reg.learner.list <- list(
    mlr3::LearnerRegrFeatureless$new(),
    mlr3tuning::auto_tuner(
      learner = Tlrn,
      tuner = mlr3tuning::tnr("internal"),
      resampling = mlr3::rsmp("insample"),
      measure = mlr3::msr("internal_valid_score", minimize = TRUE),
      term_evals = 1,
      id="torch_linear",
      store_models = TRUE))
  pkg.proj.dir <- tempfile()
  get_history_weights <- function(L){
    if(grepl("torch", L$id)){
      V <- L$tuning_result$internal_tuned_values[[1]]
      M <- L$archive$learners(1)[[1]]$model
      arr_list <- lapply(M$network$parameters, torch::as_array)
      list(
        history=M$callbacks$history,
        weights=do.call(data.table, arr_list))
    }
  }
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")),
    save_learner = get_history_weights)
  grid_jobs <- fread(file.path(pkg.proj.dir, "grid_jobs.csv"))
  one_test_N <- grid_jobs[test.fold==1 & learner.i==1 & task.i==1, .(test.subset, n.train.groups)]
  expect_equal(one_test_N$n.train.groups, c(30, 15, 10, 5))
  expect_true(all(grid_jobs$status=="not started"))
  expected_base <- length(reg.task.list)*kfold$param_set$values$folds*length(people)*nchar(subsets)
  expected_epochs <- n.epochs*expected_base
  expected_jobs <- length(reg.learner.list)*expected_base
  expect_equal(nrow(grid_jobs), expected_jobs)
  results.csv <- file.path(pkg.proj.dir, "results.csv")
  expect_false(file.exists(results.csv))
  row1 <- mlr3resampling::proj_compute(1, pkg.proj.dir)
  expect_null(row1$learner[[1]])
  computed <- mlr3resampling::proj_compute_all(pkg.proj.dir)
  history_dt <- fread(file.path(pkg.proj.dir, "learners_history.csv"))
  expected_history_cols <- c(
    "grid_job_i", "epoch", "train.regr.rmse", "valid.regr.rmse")
  expect_identical(names(history_dt), expected_history_cols)
  expect_equal(nrow(history_dt), expected_epochs)
  weights_dt <- fread(file.path(pkg.proj.dir, "learners_weights.csv"))
  expected_weights_cols <- c(
    "grid_job_i", "0.weight.V1", "0.bias")
  expect_identical(names(weights_dt), expected_weights_cols)
  expect_equal(nrow(weights_dt), expected_base)
  test_out <- mlr3resampling::proj_test(pkg.proj.dir)
  expect_equal(max(test_out$learners_history.csv$epoch), 2)
})

if(mlr3torch_available && requireNamespace("mlr3pipelines"))test_that("mlr3torch graph learner", {
  N <- 80
  set.seed(1)
  people <- c("Alice","Bob")
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(people, each=0.5*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  kfold$param_set$values$folds <- 2
  subsets <- "SA"
  kfold$param_set$values$subsets <- subsets
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
  n.epochs <- 3
  measure_list <- mlr3::msrs(c("regr.rmse", "regr.mae"))
  po_list <- list(
    mlr3pipelines::po(
      "select",
      selector = mlr3pipelines::selector_type(c("numeric", "integer"))),
    mlr3torch::PipeOpTorchIngressNumeric$new(),
    mlr3torch::nn("linear", out_features=1),
    mlr3pipelines::po(
      "torch_loss",
      mlr3torch::t_loss("mse")),
    mlr3pipelines::po(
      "torch_optimizer",
      mlr3torch::t_opt("sgd", lr=0.1)),
    mlr3pipelines::po(
      "torch_callbacks",
      mlr3torch::t_clbk("history")),
    mlr3pipelines::po(
      "torch_model_regr",
      batch_size = 100000,
      patience=n.epochs,
      measures_valid=measure_list,
      measures_train=measure_list,
      epochs = paradox::to_tune(upper = n.epochs, internal = TRUE)))
  graph <- Reduce(mlr3pipelines::concat_graphs, po_list)
  glearner <- mlr3::as_learner(graph)
  mlr3::set_validate(glearner, validate = 0.5)
  reg.learner.list <- list(mlr3tuning::auto_tuner(
    learner = glearner,
    tuner = mlr3tuning::tnr("internal"),
    resampling = mlr3::rsmp("insample"),
    measure = mlr3::msr("internal_valid_score", minimize = TRUE),
    term_evals = 1,
    id="torch_linear",
    store_models = TRUE))
  get_history_graph <- function(x){
    M <- x$archive$learners(1)[[1]]$model
    at <- grep("torch_model", ls(M), value=TRUE)
    M[[at]]$model$callbacks$history
  }
  pkg.proj.dir <- tempfile()
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    score_args=measure_list,
    save_learner = get_history_graph)
  test_out_max <- mlr3resampling::proj_test(
    pkg.proj.dir,
    max_jobs = 1)
  expect_identical(names(test_out_max), c("grid_jobs.csv", "learners.csv", "results.csv"))
  expect_equal(max(test_out_max$learners.csv$epoch), 2)
  expect_equal(nrow(test_out_max$results.csv), 1)
  task.rds.vec <- Sys.glob(file.path(pkg.proj.dir,"test","tasks", "*.rds"))
  expect_identical(basename(task.rds.vec), "1.rds")
  edit_learner_graph <- function(L){
    L$learner$base_learner()$param_set$set_values(
      patience=1,
      epochs=paradox::to_tune(upper=1, internal=TRUE))
  }
  test_out <- mlr3resampling::proj_test(
    pkg.proj.dir,
    edit_learner = edit_learner_graph)
  expect_identical(names(test_out), c("grid_jobs.csv", "learners.csv", "results.csv"))
  expect_equal(max(test_out$learners.csv$epoch), 1)
  expect_equal(nrow(test_out$results.csv), 2)
  computed <- mlr3resampling::proj_compute_all(pkg.proj.dir)
  full_out <- mlr3resampling::proj_fread(pkg.proj.dir)
  expect_identical(names(full_out), c("grid_jobs.csv", "learners.csv", "results.csv"))
  expect_equal(max(full_out$learners.csv$epoch), 3)
})

if(mlr3torch_available)test_that("mlr3torch module learner", {
  N <- 80
  set.seed(1)
  people <- c("Alice","Bob")
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(people, each=0.5*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  kfold$param_set$values$folds <- 2
  subsets <- "SA"
  kfold$param_set$values$subsets <- subsets
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
  measure_list <- mlr3::msrs(c("regr.rmse", "regr.mae"))
  nn_one_layer = torch::nn_module(
    "nn_one_layer",
    initialize = function(task, size_hidden) {
      self$first = torch::nn_linear(task$n_features, size_hidden)
      self$second = torch::nn_linear(size_hidden, 1)
    },
    # argument x corresponds to the ingress token x
    forward = function(x) {
      x = self$first(x)
      x = torch::nnf_relu(x)
      self$second(x)
    }
  )
  n.epochs <- 3
  learner = mlr3::lrn(
    "regr.module",
    module_generator = nn_one_layer,
    ingress_tokens = list(x = mlr3torch::ingress_num()),
    epochs = paradox::to_tune(upper=n.epochs, internal=TRUE),
    patience = n.epochs,
    size_hidden = 20,
    batch_size = 16)
  learner$param_set$values[
    paste0("measures_",c("train","valid"))
  ] <- mlr3::msrs("regr.rmse")
  learner$callbacks <- mlr3torch::t_clbk("history")
  mlr3::set_validate(learner, validate = 0.5)
  reg.learner.list <- list(mlr3tuning::auto_tuner(
    learner = learner,
    tuner = mlr3tuning::tnr("internal"),
    resampling = mlr3::rsmp("insample"),
    measure = mlr3::msr("internal_valid_score", minimize = TRUE),
    term_evals = 1,
    id="torch_dense",
    store_models = TRUE))
  get_history_module <- function(x){
    x$archive$learners(1)[[1]]$model$callbacks$history
  }
  pkg.proj.dir <- tempfile()
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    score_args=measure_list,
    save_learner = get_history_module)
  test_out <- mlr3resampling::proj_test(pkg.proj.dir)
  expect_identical(names(test_out), c("grid_jobs.csv", "learners.csv", "results.csv"))
  expect_equal(max(test_out$learners.csv$epoch), 2)
  computed <- mlr3resampling::proj_compute_all(pkg.proj.dir)
  full_out <- mlr3resampling::proj_fread(pkg.proj.dir)
  expect_identical(names(full_out), c("grid_jobs.csv", "learners.csv", "results.csv"))
  expect_equal(max(full_out$learners.csv$epoch), 3)
})

if(mlr3torch_available && requireNamespace("glmnet"))test_that("torch and glmnet testing and interpretation", {
  stask <- mlr3::tsk("sonar")
  stask$col_roles$stratum <- "Class"
  kfold <- mlr3::ResamplingCV$new()
  kfold$param_set$values$folds <- 2
  gen_linear <- torch::nn_module(
    "my_linear",
    initialize = function(task) {
      self$weight = torch::nn_linear(task$n_features, 1)
    },
    forward = function(x) {
      self$weight(x)
    }
  )
  learner_list <- list(
    mlr3resampling::AutoTunerTorch_epochs$new(
      "torch_linear",
      module_generator=gen_linear,
      max_epochs=3,
      batch_size=10,
      measure_list=mlr3::msrs("classif.auc")
    ),
    mlr3resampling::LearnerClassifCVGlmnetSave$new()
  )
  pkg.proj.dir <- tempfile()
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    stask,    
    learner_list,
    score_args=mlr3::msrs(c("classif.auc","classif.acc")),
    kfold)
  N_minor <- 30
  ctab <- table(stask$data(cols="Class"))
  etab <- floor(ctab/ctab[["R"]]*N_minor)
  test_out <- mlr3resampling::proj_test(
    pkg.proj.dir, min_samples_per_stratum = N_minor)
  expect_equal(nrow(test_out$learners_weights.csv), 60)
  expect_equal(nrow(test_out$learners_history.csv), 2)
  expect_identical(test_out$results.csv$learner_id, c("torch_linear", "classif.cv_glmnet"))
  expect_equal(sum(is.finite(test_out$results.csv$classif.auc)), 2)
  test_task <- readRDS(file.path(pkg.proj.dir, "test", "tasks", "1.rds"))
  Class_dt <- test_task$data(cols="Class")
  Class_tab <- table(Class_dt$Class)
  expect_equal(as.numeric(Class_tab), as.numeric(etab))
})
