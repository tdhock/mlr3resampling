library(testthat)
library(data.table)

set.seed(1)
N <- 60L
subset_name <- "Female cohort with long text"
task.dt <- data.table(
  x=runif(N, -2, 2),
  subset_col=factor(rep(c(subset_name, "Male cohort with long text"), each=N/2))
)[, y := x^2 + rnorm(.N, sd=0.2)][]
reg.task <- mlr3::TaskRegr$new("toy_soak", task.dt, target="y")
reg.task$col_roles$feature <- "x"
reg.task$col_roles$subset <- "subset_col"
reg.task$col_roles$stratum <- "subset_col"
soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
soak$param_set$values$folds <- 2L
soak$param_set$values$seeds <- 1L
soak$param_set$values$sizes <- 0L
reg.learner.list <- list(
  mlr3::LearnerRegrFeatureless$new()
)
if(requireNamespace("rpart", quietly=TRUE)){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}
bench.grid <- mlr3::benchmark_grid(
  reg.task,
  reg.learner.list,
  soak
)
bench.result <- mlr3::benchmark(bench.grid)
score.dt <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
model_name <- unique(score.dt$algorithm)[1]

test_that("pvalue_downsample returns strict S3 object", {
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_s3_class(down.list, "pvalue_downsample")
  expect_identical(as.character(down.list$subset_name), subset_name)
  expect_identical(down.list$model_name, model_name)
  expect_identical(down.list$value.var, "regr.rmse")
})

test_that("pvalue_downsample picks first-row subset when multiple subsets exist", {
  score_in <- score.dt[algorithm == model_name]
  expected_subset <- score_in$test.subset[[1]]
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_identical(down.list$subset_name, expected_subset)
})

test_that("pvalue_downsample handles input without downsample rows", {
  score_in <- data.table(score.dt)[
  , n.train.groups := groups
  ][test.subset == subset_name & algorithm == model_name]
  expect_warning(
    down.list <- mlr3resampling::pvalue_downsample(score_in),
    "duplicate row/column combinations"
  )
  expect_s3_class(down.list, "pvalue_downsample")
})

test_that("pvalue_downsample errors when there is no comparison subset", {
  score_in <- score.dt[train.subsets == "same"][
  , n.train.groups := pmax(1L, groups - 1L)
  ][test.subset == subset_name & algorithm == model_name]
  expect_error({
    mlr3resampling::pvalue_downsample(score_in)
  }, "must contain at least one of: all, other")
})

test_that("pvalue_downsample auto-selects first metric when multiple metric columns are present", {
  score_in <- data.table(score.dt)[
  , classif.ce := 0.2
  ][test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_identical(down.list$value.var, "regr.rmse")
})

test_that("pvalue_downsample supports value.var and digits arguments", {
  score_in <- data.table(score.dt)[
  , RMSE := regr.rmse
  ][test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(
    score_in,
    value.var="RMSE",
    digits=2
  )
  expect_identical(down.list$value.var, "RMSE")
  num_pat <- "[0-9]+\\.[0-9]{2}"
  pattern <- paste0("^", num_pat, " \u00B1 ", num_pat, "(, N = [0-9]+)?$")
  expect_match(down.list$stats$text_label, pattern)
})

test_that("plot.pvalue_downsample returns ggplot", {
  skip_if_not_installed("ggplot2")
  score_in <- data.table(score.dt)[
    test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  down.plot <- plot(down.list)
  expect_s3_class(down.plot, "ggplot")
})

test_that("pvalue_downsample end-to-end with real SOAK sizes=0 result", {
  skip_if_not_installed("ggplot2")
  score_in <- data.table(score.dt)[
    test.subset == subset_name & algorithm == model_name]
  min.groups <- min(score_in$groups)
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_s3_class(down.list, "pvalue_downsample")
  expect_setequal(unique(down.list$stats$sample_size), c("full", min.groups))
  expect_setequal(unique(down.list$stats$Train_subsets), c("all", "other", "same"))
  expect_setequal(unique(down.list$pvalues$Train_subsets), c("all-same", "other-same"))
  down.plot <- plot(down.list)
  expect_s3_class(down.plot, "ggplot")
})
