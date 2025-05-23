library(testthat)
library(data.table)
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
