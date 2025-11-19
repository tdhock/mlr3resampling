library(testthat)
library(data.table)

test_that("mlr3torch history saved", {
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
    "groups", "test.fold", "seed", "n.train.groups", "iteration", "Train_subsets")
  expect_identical(names(csv_data_list$learners.csv), expected_join_cols)
})

test_that("mlr3torch history and weights saved", {
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

test_that("mlr3torch graph learner", {
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

test_that("mlr3torch module learner", {
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

test_that("torch and glmnet testing and interpretation", {
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

