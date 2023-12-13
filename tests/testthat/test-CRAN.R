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
