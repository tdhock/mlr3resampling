library(testthat)
library(data.table)

make_downsample_score <- function(){
  subset_name <- "Female cohort with long text"
  full.groups <- c(same=6L, other=4L, all=10L)
  min.groups <- min(full.groups)
  dt.list <- list()
  for(train.subset in names(full.groups)){
    full.n <- full.groups[[train.subset]]
    train.sizes <- unique(c(full.n, min.groups))
    for(test.fold in 1:4){
      for(n.train.groups in train.sizes){
        is.small <- n.train.groups < full.n
        base <- switch(
          train.subset,
          same=0.84,
          other=0.82,
          all=0.80
        )
        dt.list[[paste(train.subset, test.fold, n.train.groups)]] <- data.table(
          task_id="toy",
          test.subset=subset_name,
          train.subsets=train.subset,
          test.fold=test.fold,
          algorithm="rpart",
          groups=full.n,
          n.train.groups=n.train.groups,
          regr.rmse=base+ifelse(is.small, 0.03, 0)+test.fold*0.001
        )
      }
    }
  }
  rbindlist(dt.list)
}

test_that("pvalue_downsample returns strict S3 object", {
  score.dt <- make_downsample_score()
  down.list <- mlr3resampling::pvalue_downsample(
    score.dt,
    "Female cohort with long text",
    "rpart"
  )
  expect_s3_class(down.list, "pvalue_downsample")
  expect_identical(down.list$subset_name, "Female cohort with long text")
  expect_identical(down.list$model_name, "rpart")
  expect_identical(down.list$value.var, "regr.rmse")
  expect_identical(levels(down.list$stats$sample_size), c("full", "4"))
  expect_true(all(c("stats", "pvalues") %in% names(down.list)))
  expect_true(all(c("sample_size", "Train_subsets") %in% names(down.list$stats)))
  expect_true(all(c("sample_size", "Train_subsets") %in% names(down.list$pvalues)))
})

test_that("pvalue_downsample uses exact subset match", {
  score.dt <- make_downsample_score()
  expect_error(
    mlr3resampling::pvalue_downsample(score.dt, "Female cohort", "rpart"),
    "was not found in score_in\\$test.subset"
  )
})

test_that("pvalue_downsample errors when there is no downsample", {
  score.dt <- make_downsample_score()[, n.train.groups := groups][]
  expect_error(
    mlr3resampling::pvalue_downsample(score.dt, "Female cohort with long text", "rpart"),
    "scores do not have downsamples",
    fixed=TRUE
  )
})

test_that("pvalue_downsample errors when there is no comparison subset", {
  score.dt <- make_downsample_score()[train.subsets == "same"]
  expect_error(
    mlr3resampling::pvalue_downsample(score.dt, "Female cohort with long text", "rpart"),
    "must contain at least one comparison subset"
  )
})

test_that("pvalue_downsample errors when multiple metric columns are present", {
  score.dt <- make_downsample_score()[, classif.ce := 0.2][]
  expect_error(
    mlr3resampling::pvalue_downsample(score.dt, "Female cohort with long text", "rpart"),
    "exactly one metric column matching classif\\|regr"
  )
})

test_that("pvalue_downsample uses exact model match", {
  score.dt <- make_downsample_score()
  expect_error(
    mlr3resampling::pvalue_downsample(score.dt, "Female cohort with long text", "tree"),
    "was not found in score_in\\$algorithm"
  )
})

test_that("pvalue_downsample supports value.var and digits arguments", {
  score.dt <- make_downsample_score()[, RMSE := regr.rmse][]
  down.list <- mlr3resampling::pvalue_downsample(
    score.dt,
    "Female cohort with long text",
    "rpart",
    value.var="RMSE",
    digits=2
  )
  expect_identical(down.list$value.var, "RMSE")
  expect_true(all(grepl("^[0-9]+\\.[0-9]{2} \u00B1 [0-9]+\\.[0-9]{2}$", down.list$stats$text_label)))
})

test_that("plot.pvalue_downsample returns ggplot", {
  skip_if_not_installed("ggplot2")
  score.dt <- make_downsample_score()
  down.list <- mlr3resampling::pvalue_downsample(
    score.dt,
    "Female cohort with long text",
    "rpart"
  )
  down.plot <- plot(down.list)
  expect_s3_class(down.plot, "ggplot")
})

test_that("pvalue_downsample end-to-end with real SOAK sizes=0 result", {
  skip_if_not_installed("ggplot2")
  set.seed(1)
  N <- 40L
  person <- factor(rep(c("F", "M"), each=N/2))
  task.dt <- data.table(
    x=runif(N, -2, 2),
    person=person
  )[, y := x^2 + rnorm(N, sd=0.2)][]
  reg.task <- mlr3::TaskRegr$new("toy_soak", task.dt, target="y")
  reg.task$col_roles$feature <- "x"
  reg.task$col_roles$subset <- "person"
  reg.task$col_roles$stratum <- "person"
  soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  soak$param_set$values$folds <- 2L
  soak$param_set$values$seeds <- 1L
  soak$param_set$values$sizes <- 0L
  bench.grid <- mlr3::benchmark_grid(
    reg.task,
    list(mlr3::LearnerRegrFeatureless$new()),
    soak
  )
  bench.result <- mlr3::benchmark(bench.grid)
  score.dt <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
  model_name <- unique(score.dt$algorithm)[1]
  expect_true(any(score.dt[test.subset == "F", n.train.groups < groups]))
  min.groups <- as.character(min(score.dt[test.subset == "F", groups]))
  down.list <- mlr3resampling::pvalue_downsample(score.dt, "F", model_name)
  expect_s3_class(down.list, "pvalue_downsample")
  expect_true(all(c("full", min.groups) %in% unique(down.list$stats$sample_size)))
  expect_true("same" %in% as.character(unique(down.list$stats$Train_subsets)))
  expect_true(any(grepl("-same$", as.character(unique(down.list$pvalues$Train_subsets)))))
  down.plot <- plot(down.list)
  expect_s3_class(down.plot, "ggplot")
})
