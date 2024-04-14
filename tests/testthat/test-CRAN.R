library(testthat)
library(data.table)
test_that("resampling error if no group", {
  itask <- mlr3::TaskClassif$new("iris", iris, target="Species")
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_error({
    same_other$instantiate(itask)
  }, 'task has no group, but at least one group variable is required; use task$set_col_roles(group_col, c("group","stratum"))', fixed=TRUE)
})

test_that("resampling error if no strata", {
  iris.dt <- data.table(iris)[, g := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("g", "group")
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_error({
    same_other$instantiate(itask)
  }, 'task has no strata, but at least one stratum variable is required; at least assign the group variable to a stratum, task$set_col_roles(group_col, c("group","stratum"))', fixed=TRUE)
})

test_that("instantiation creates instance", {
  iris.dt <- data.table(iris)[, g := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("g", c("stratum","group"))
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  same_other$instantiate(itask)
  expect_identical(same_other$instance$id.dt$g, iris.dt$g)
})

test_that("error for group named group", {
  iris.dt <- data.table(iris)[, group := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("group", c("stratum","group"))
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role group must not be named group; please fix by renaming group col")
})

test_that("error for group named row_id", {
  iris.dt <- data.table(iris)[, row_id := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("row_id", c("stratum","group"))
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role group must not be named row_id; please fix by renaming row_id col")
})

test_that("error for group named fold", {
  iris.dt <- data.table(iris)[, fold := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("fold", c("stratum","group"))
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role group must not be named fold; please fix by renaming fold col")
})

test_that("error for group named display_row", {
  iris.dt <- data.table(iris)[, display_row := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("display_row", c("stratum","group"))
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role group must not be named display_row; please fix by renaming display_row col")
})

test_that("error for group named test", {
  iris.dt <- data.table(iris)[, test := rep(1:3, l=.N)]
  itask <- mlr3::TaskClassif$new("iris", iris.dt, target="Species")
  itask$set_col_roles("test", c("stratum","group"))
  same_other <- mlr3resampling::ResamplingSameOtherCV$new()
  expect_identical(same_other$instance, NULL)
  expect_error({
    same_other$instantiate(itask)
  }, "col with role group must not be named test; please fix by renaming test col")
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
test_that("ResamplingSameOtherSizesCV OK", {
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
  prop.tab <- group.tab/sum(group.tab)
  reg.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  reg.task$col_roles$group_generalization <- "random_group"
  reg.task$col_roles$group_atomic <- "agroup"
  reg.task$col_roles$stratum <- "random_group"
  reg.task$col_roles$feature <- "x"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  same_other_sizes_cv$instantiate(reg.task)
  same.dt <- same_other_sizes_cv$instance$iteration.dt[
    test.fold==1 & seed==1 & test.group=="A" & train.groups=="same"]
  same.dt[, expect_identical(test[[1]], test[[2]])]
  same.dt[, expect_equal(sapply(train, length), n.train.atoms*atomic.group.size)]
  same.dt[, expect_in(train[[1]], train[[2]])]
  all.dt <- same_other_sizes_cv$instance$iteration.dt[
    test.fold==1 & seed==1 & test.group=="A" & train.groups=="all"]
  all.dt[, expect_identical(test[[1]], test[[2]])]
  all.dt[, expect_in(train[[1]], train[[2]])]
  tab.counts <- sapply(all.dt$train, function(i)table(task.dt$random_group[i]))
  expected.counts <- matrix(
    all.dt$n.train.atoms*atomic.group.size,
    length(prop.tab), nrow(all.dt), byrow=TRUE
  )*as.numeric(prop.tab)
  expect_true(all(expected.counts-tab.counts < atomic.group.size))
  agroup.count.vec <- sapply(
    all.dt$train, function(i)table(table(task.dt$agroup[i])))
  expect_identical(names(agroup.count.vec), rep(as.character(atomic.group.size), nrow(all.dt)))
  ## no atomic group.
  no.task <- mlr3::TaskRegr$new(
    "sin", task.dt, target="y")
  no.task$col_roles$group_generalization <- "random_group"
  no.task$col_roles$stratum <- "random_group"
  no.task$col_roles$feature <- "x"
  same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  same_other_sizes_cv$instantiate(no.task)
  same.dt <- same_other_sizes_cv$instance$iteration.dt[
    test.fold==1 & seed==1 & test.group=="A" & train.groups=="same"]
  same.dt[, expect_identical(test[[1]], test[[2]])]
  same.dt[, expect_equal(sapply(train, length), n.train.atoms)]
  same.dt[, expect_in(train[[1]], train[[2]])]
  all.dt <- same_other_sizes_cv$instance$iteration.dt[
    test.fold==1 & seed==1 & test.group=="A" & train.groups=="all"]
  all.dt[, expect_identical(test[[1]], test[[2]])]
  all.dt[, expect_in(train[[1]], train[[2]])]
  tab.counts <- sapply(all.dt$train, function(i)table(task.dt$random_group[i]))
  expected.counts <- matrix(
    all.dt$n.train.atoms,
    length(prop.tab), nrow(all.dt), byrow=TRUE
  )*as.numeric(prop.tab)
  expect_true(all(expected.counts-tab.counts < 1))
  agroup.count.mat <- sapply(#1 sometimes since no atomic grouping.
    all.dt$test, function(i)table(table(task.dt$agroup[i])))
  expect_identical(rownames(agroup.count.mat), c("1","2"))
})
