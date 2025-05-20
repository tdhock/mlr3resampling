ResamplingBase = R6::R6Class(
  "Resampling",
  public = list(
    id = NULL,
    label = NULL,
    param_set = NULL,
    instance = NULL,
    task_hash = NA_character_,
    task_nrow = NA_integer_,
    task_row_hash = NA_character_,
    man = NULL,
    initialize = function(id, param_set = ps(), label = NA_character_, man = NA_character_) {
      self$id = checkmate::assert_string(id, min.chars = 1L)
      self$label = checkmate::assert_string(label, na.ok = TRUE)
      self$param_set = paradox::assert_param_set(param_set)
      self$man = checkmate::assert_string(man, na.ok = TRUE)
    },
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
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
    help = function() {
      self$man
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
    },
    is_instantiated = function(rhs) {
      !is.null(self$instance)
    },
    hash = function(rhs) {
      if (!self$is_instantiated) {
        return(NA_character_)
      }
      mlr3misc::calculate_hash(list(
        class(self),
        self$id,
        self$param_set$values,
        self$instance))
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
