ResamplingVariableSizeTrain = R6::R6Class(
  "ResamplingVariableSizeTrain",
  inherit=ResamplingBase,
  public = list(
    instantiate = function(task) {
      task = mlr3::assert_task(mlr3::as_task(task))
      reserved.names <- c(
        "row_id", "fold", "group", "display_row",
        "train.groups", "test.fold", "test.group", "iteration", 
        "test", "train", "algorithm", "uhash", "nr", "task", "task_id",
        "learner", "learner_id", "resampling", "resampling_id",
        "prediction")
      ## bad.names <- group.name.vec[group.name.vec %in% reserved.names]
      ## if(length(bad.names)){
      ##   first.bad <- bad.names[1]
      ##   stop(sprintf("col with role group must not be named %s; please fix by renaming %s col", first.bad, first.bad))
      ## }
      ## orig.group.dt <- task$data(cols=group.name.vec)
      strata <- if(is.null(task$strata)){
        data.dt <- task$data()
        data.table(N=nrow(data.dt), row_id=list(1:nrow(data.dt)))
      }else task$strata
      folds = private$.combine(
        lapply(strata$row_id, private$.sample, task = task)
      )[order(row_id)]
      min_train_data <- self$param_set$values[["min_train_data"]]
      uniq.folds <- unique(folds$fold)
      iteration.dt.list <- list()
      for(test.fold in uniq.folds){
        is.set.fold <- list(
          test=folds[["fold"]] == test.fold)
        is.set.fold[["train"]] <- !is.set.fold[["test"]]
        i.set.list <- lapply(is.set.fold, which)
        max_train_data <- length(i.set.list$train)
        log.range.data <- log(c(min_train_data, max_train_data))
        seq.args <- c(as.list(log.range.data), list(l=self$param_set$values[["train_sizes"]]))
        log.train.sizes <- do.call(seq, seq.args)
        train.size.vec <- unique(as.integer(exp(log.train.sizes)))
        for(seed in 1:self$param_set$values[["random_seeds"]]){
          set.seed(seed)
          ord.i.vec <- sample(i.set.list$train)
          iteration.dt.list[[paste(test.fold, seed)]] <- data.table(
            test.fold,
            seed,
            train_size=train.size.vec,
            train=lapply(train.size.vec, function(last)ord.i.vec[1:last]),
            test=list(i.set.list$test))
        }
      }
      self$instance <- list(
        iteration.dt=rbindlist(iteration.dt.list)[, iteration := .I][],
        id.dt=folds)
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  )
)

ResamplingVariableSizeTrainCV = R6::R6Class(
  "ResamplingVariableSizeTrainCV",
  inherit = ResamplingVariableSizeTrain,
  public = list(
    initialize = function() {
      ps = paradox::ps(
        folds = paradox::p_int(2L, tags = "required"),
        min_train_data=paradox::p_int(1L, tags = "required"),
        random_seeds=paradox::p_int(1L, tags = "required"),
        train_sizes = paradox::p_int(2L, tags = "required"))
      ps$values = list(
        folds = 3L,
        min_train_data=10L,
        random_seeds=3L,
        train_sizes=5L)
      super$initialize(
        id = "variable_size_train_cv",
        param_set = ps,
        label = "Cross-Validation with variable size train sets",
        man = "ResamplingVariableSizeTrainCV")
    }
  ),
  active = list(
    iters = function(rhs) {
      nrow(self$instance$iteration.dt)
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      data.table(
        row_id = ids,
        fold = sample(
          seq(0, length(ids)-1) %%
            as.integer(self$param_set$values$folds) + 1L
        ),
        key = "fold"
      )
    },
    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },
    deep_clone = function(name, value) {
      switch(name,
        "instance" = copy(value),
        "param_set" = value$clone(deep = TRUE),
        value
        )
    }
  )
)
