library(testthat)
library(data.table)

get_soak_score <- local({
  cache <- NULL
  function(){
    if(!is.null(cache)){
      return(copy(cache))
    }
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
    cache <<- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
    copy(cache)
  }
})

test_that("pvalue_downsample returns strict S3 object", {
  score.dt <- get_soak_score()
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(
    score_in
  )
  expect_s3_class(down.list, "pvalue_downsample")
  expect_identical(down.list$subset_name, subset_name)
  expect_identical(down.list$model_name, model_name)
  expect_identical(down.list$value.var, "regr.rmse")
})

test_that("pvalue_downsample picks first-row subset when multiple subsets exist", {
  score.dt <- get_soak_score()
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[algorithm == model_name]
  expected_subset <- as.character(score_in$test.subset[[1]])
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_identical(down.list$subset_name, expected_subset)
})

test_that("pvalue_downsample handles input without downsample rows", {
  score.dt <- get_soak_score()[, n.train.groups := groups][]
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  down.list <- suppressWarnings(mlr3resampling::pvalue_downsample(score_in))
  expect_s3_class(down.list, "pvalue_downsample")
})

test_that("pvalue_downsample errors when there is no comparison subset", {
  score.dt <- get_soak_score()[train.subsets == "same"][
    , n.train.groups := pmax(1L, groups - 1L)
  ][]
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  expect_error(
    mlr3resampling::pvalue_downsample(score_in),
    "must contain at least one of: all, other"
  )
})

test_that("pvalue_downsample auto-selects first metric when multiple metric columns are present", {
  score.dt <- get_soak_score()[, classif.ce := 0.2][]
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_identical(down.list$value.var, "regr.rmse")
})

test_that("pvalue_downsample supports value.var and digits arguments", {
  score.dt <- get_soak_score()[, RMSE := regr.rmse][]
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(
    score_in,
    value.var="RMSE",
    digits=2
  )
  expect_identical(down.list$value.var, "RMSE")
  expect_true(all(grepl(
    "^[0-9]+\\.[0-9]{2} \u00B1 [0-9]+\\.[0-9]{2}(, N = [0-9]+)?$",
    down.list$stats$text_label
  )))
})

test_that("plot.pvalue_downsample returns ggplot", {
  skip_if_not_installed("ggplot2")
  score.dt <- get_soak_score()
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  down.list <- mlr3resampling::pvalue_downsample(
    score_in
  )
  down.plot <- plot(down.list)
  expect_s3_class(down.plot, "ggplot")
})

test_that("pvalue_downsample end-to-end with real SOAK sizes=0 result", {
  skip_if_not_installed("ggplot2")
  score.dt <- get_soak_score()
  subset_name <- "Female cohort with long text"
  model_name <- unique(score.dt$algorithm)[1]
  score_in <- score.dt[test.subset == subset_name & algorithm == model_name]
  expect_true(any(score_in$n.train.groups < score_in$groups))
  min.groups <- as.character(min(score_in$groups))
  down.list <- mlr3resampling::pvalue_downsample(score_in)
  expect_s3_class(down.list, "pvalue_downsample")
  expect_true(all(c("full", min.groups) %in% unique(down.list$stats$sample_size)))
  expect_true("same" %in% as.character(unique(down.list$stats$Train_subsets)))
  expect_true(any(grepl("-same$", as.character(unique(down.list$pvalues$Train_subsets)))))
  down.plot <- plot(down.list)
  expect_s3_class(down.plot, "ggplot")
})
