ResamplingVariableSizeTrainCV = R6::R6Class(
  "ResamplingVariableSizeTrainCV",
  inherit=ResamplingBase,
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
    },
    get_instance = function(task) {
      row_id <- fold <- prop <- . <- row_seed <- iteration <- train_min_size <- train_size <- train_size_i <- NULL
      ## Above to avoid CRAN NOTEs.
      strata <- if(is.null(task$strata)){
        data.table(N=task$nrow, row_id=list(seq_len(task$nrow)))
      }else task$strata
      strata.list <- lapply(strata$row_id, private$.sample, task = task)
      folds = private$.combine(strata.list)[order(row_id)]
      max.train.vec <- sapply(strata.list, nrow)
      small.strat.i <- which.min(max.train.vec)
      min_train_data <- self$param_set$values[["min_train_data"]]
      uniq.folds <- sort(unique(folds$fold))
      iteration.dt.list <- list()
      for(test.fold in uniq.folds){
        train.strata.list <- lapply(strata.list, function(DT)DT[fold != test.fold])
        max_train_data <- nrow(train.strata.list[[small.strat.i]])
        if(max_train_data <= min_train_data){
          stop(sprintf(
            "max_train_data=%d (in smallest stratum) but should be larger than min_train_data=%d, please fix by decreasing min_train_data",
            max_train_data, min_train_data))
        }
        log.range.data <- log(c(min_train_data, max_train_data))
        seq.args <- c(as.list(log.range.data), list(l=self$param_set$values[["train_sizes"]]))
        log.train.sizes <- do.call(seq, seq.args)
        train.size.vec <- as.integer(round(exp(log.train.sizes)))
        size.tab <- table(train.size.vec)
        if(any(size.tab>1)){
          stop("train sizes not unique, please decrease train_sizes")
        }
        for(seed in 1:self$param_set$values[["random_seeds"]]){
          set.seed(seed)
          train.seed.list <- lapply(train.strata.list, function(DT)DT[sample(.N)][, `:=`(
            row_seed = .I,
            prop = .I/.N
          )][])
          test.index.vec <- do.call(c, lapply(
            strata.list, function(DT)DT[fold == test.fold, row_id]))
          train.prop.dt <- train.seed.list[[small.strat.i]][train.size.vec, data.table(prop)]
          train.i.list <- lapply(train.seed.list, function(DT)DT[
            train.prop.dt,
            .(train.i=lapply(row_seed, function(last)DT$row_id[1:last])),
            on="prop",
            roll="nearest"])
          train.index.list <- list()
          for(train.size.i in seq_along(train.size.vec)){
            strata.index.list <- lapply(train.i.list, function(DT)DT[["train.i"]][[train.size.i]])
            train.index.list[[train.size.i]] <- do.call(c, strata.index.list)
          }
          iteration.dt.list[[paste(test.fold, seed)]] <- data.table(
            test.fold,
            seed,
            small_stratum_size=train.size.vec,
            train_size_i=seq_along(train.size.vec),
            train_size=sapply(train.index.list, length),
            train=train.index.list,
            test=list(test.index.vec))
        }
      }
      list(
        iteration.dt=rbindlist(
          iteration.dt.list
        )[
        , iteration := .I
        ][
        , train_min_size := min(train_size), by=train_size_i
        ][],
        id.dt=folds)
    }
  )
)
