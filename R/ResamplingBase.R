ResamplingBase = R6::R6Class(
  "ResamplingBase",
  inherit = mlr3::Resampling,
  public = list(
    instantiate = function(task) {
      task = mlr3::assert_task(mlr3::as_task(task))
      self$instance = self$get_instance(task)
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      self$task_row_hash = task$row_hash
      invisible(self)
    },
    print = function(...) {
      cat(
        format(self),
        if (is.null(self$label) || is.na(self$label))
          "" else paste0(": ", self$label)
      )
      cat("\n* Iterations:", self$iters)
      cat("\n* Instantiated:", self$is_instantiated)
      cat("\n* Parameters:\n")
      str(self$param_set$values)
    },
    train_set = function(i) {
      self$instance$iteration.dt$train[[i]]
    },
    test_set = function(i) {
      self$instance$iteration.dt$test[[i]]
    }
  ),
  active = list(
    iters = function(rhs) {
      nrow(self$instance$iteration.dt)
    }
  ),
  private=list(
    .sample = function(ids, ...) {
      data.table(
        row_id = ids,
        fold = sample(
          seq(0, length(ids)-1) %%
            as.integer(self$param_set$values$folds) + 1L
        ),
        key = "fold"
      )
    }
  )
)
