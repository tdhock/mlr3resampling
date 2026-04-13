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
soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
reg.task$col_roles$subset <- "subset_col"
reg.task$col_roles$stratum <- "subset_col"
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
  expect_identical(down.list$value.var, "regr.rmse")
})

test_that("pvalue_downsample picks first row subset when multiple subsets exist", {
  score_in <- score.dt[algorithm == model_name]
  expect_warning(
    down.list <- mlr3resampling::pvalue_downsample(score_in),
    "duplicate row/column combinations"
  )
  expect_s3_class(down.list, "pvalue_downsample")
})

test_that("pvalue_downsample handles input without downsample rows", {
  score_in <- data.table(score.dt)[
  , n.train.groups := groups
  ][test.subset == subset_name & algorithm == model_name]
  expect_error({
    mlr3resampling::pvalue_downsample(score_in)
  }, "no downsample results, please set SOAK$param_set$sizes=0 and re-run benchmark", fixed=TRUE)
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

test_that("plot.score orders y axis by subset and sample size", {
  skip_if_not_installed("ggplot2")
  score_in <- data.table(
    task_id="toy",
    test.subset="A",
    algorithm="featureless",
    Train_subsets=c("all", "same", "other", "same", "all", "other"),
    n.train.groups=c(36L, 72L, 36L, 360L, 360L, 72L),
    regr.rmse=c(0.3, 0.2, 0.4, 0.1, 0.5, 0.25)
  )
  setattr(score_in, "class", c("score", class(score_in)))
  score.plot <- plot(score_in)
  expect_s3_class(score.plot, "ggplot")
  expect_identical(
    levels(score.plot$layers[[1]]$data$y.label),
    c("all 360", "all 36", "other 72", "other 36", "same 360", "same 72")
  )
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

test_that("pvalue_downsample fails without downsamples", {
  library(data.table)
  N <- 2400
  abs.x <- 3*pi
  set.seed(2)
  grid.dt <- data.table(
    x=seq(-abs.x,abs.x, l=201),
    y=0)
  x.vec <- runif(N, -abs.x, abs.x)
  standard.deviation.vec <- c(
    easy=0.1,
    hard=1.7)
  reg.data.list <- list()
  grid.signal.dt.list <- list()
  sim_fun <- sin
  for(difficulty in names(standard.deviation.vec)){
    standard.deviation <- standard.deviation.vec[[difficulty]]
    signal.vec <- sim_fun(x.vec)
    y <- signal.vec+rnorm(N,sd=standard.deviation)
    task.dt <- data.table(x=x.vec, y)
    reg.data.list[[difficulty]] <- data.table(difficulty, task.dt)
    grid.signal.dt.list[[difficulty]] <- data.table(
      difficulty,
      algorithm="ideal",
      x=grid.dt$x,
      y=sim_fun(grid.dt$x))
  }
  (reg.data <- rbindlist(reg.data.list))
  (grid.signal.dt <- rbindlist(grid.signal.dt.list))
  algo.colors <- c(
    featureless="blue",
    rpart="red",
    ideal="black")
  if(require(ggplot2)){
    ggplot()+
      theme_bw()+
      geom_point(aes(
        x, y),
        fill="white",
        color="grey",
        data=reg.data)+
      geom_line(aes(
        x, y, color=algorithm),
        linewidth=2,
        data=grid.signal.dt)+
      scale_color_manual(values=algo.colors)+
      facet_grid(. ~ difficulty, labeller=label_both)
  }
  (reg.learner.list <- list(
    if(requireNamespace("rpart"))mlr3::LearnerRegrRpart$new()))
  SOAKED <- mlr3resampling::ResamplingSameOtherSizesCV$new()
  SOAKED$param_set$values$folds <- 2
  set.seed(1)
  sim.meta.list <- list(
    different=rbind(
      reg.data[difficulty=="easy"][sample(.N, 400)],
      reg.data[difficulty=="hard"][sample(.N, 200)]
    )[, .(x,y,Subset=ifelse(difficulty=="easy", "large", "small"))],
    iid_easy=reg.data[
      difficulty=="easy"
    ][sample(.N, 120)][
    , Subset := rep(c("large","large","small"), l=.N)
    ][, .(x,y,Subset)])
  d_task_list <- list()
  gg_list <- list()
  for(sim.name in names(sim.meta.list)){
    sim.i.dt <- sim.meta.list[[sim.name]]
    sub_task <- mlr3::TaskRegr$new(
      sim.name, sim.i.dt, target="y")
    sub_task$col_roles$subset <- "Subset"
    sub_task$col_roles$feature <- "x"
    d_task_list[[sim.name]] <- sub_task
    if(require("ggplot2")){
      gg_list[[sim.name]] <- ggplot()+
        ggtitle(paste("Simulation:", sim.name))+
        geom_point(aes(
          x, y),
          color="black",
          fill="white",
          data=sim.i.dt)+
        geom_line(aes(
          x, y, color=algorithm),
          data=grid.signal.dt)+
        scale_color_manual(values=algo.colors)+
        facet_grid(Subset~., labeller=label_both)
    }
  }
  gg_list
  (reg.bench.grid <- mlr3::benchmark_grid(
    d_task_list$iid_easy,
    reg.learner.list,
    SOAKED))
  (reg.bench.result <- mlr3::benchmark(reg.bench.grid))
  (score_dt <- mlr3resampling::score(
    reg.bench.result, mlr3::msr("regr.rmse")
  )[, .(
    test.subset, test.fold,
    train.subsets, Train_subsets, groups, n.train.groups,
    algorithm, regr.rmse, task_id)])
  if(interactive())plot(score_dt)
  plist <- mlr3resampling::pvalue(score_dt)
  if(interactive())plot(plist)
  expect_error({
    mlr3resampling::pvalue_downsample(score_dt)
  }, "no downsample results, please set SOAK$param_set$sizes=0 and re-run benchmark", fixed=TRUE)
})
