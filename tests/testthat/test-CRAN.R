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

test_that("error for 10 data", {
  size_cv <- mlr3resampling::ResamplingVariableSizeTrainCV$new()
  i10.dt <- data.table(iris)[1:10]
  i10.task <- mlr3::TaskClassif$new("i10", i10.dt, target="Species")
  expect_error({
    size_cv$instantiate(i10.task)
  },
  "task$nrow=10 but should be larger than min_train_data=10",
  fixed=TRUE)
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
