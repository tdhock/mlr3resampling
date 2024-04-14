## copied from
## https://github.com/mlr-org/mlr3/blob/main/R/ResamplingCV.R but
## error message "cannot combine stratification with grouping" removed.
ResamplingStrataGroup = R6Class(
  "Resampling",
  public = list(
    #' @template field_label
    label = NULL,

    #' @template field_param_set
    param_set = NULL,

    #' @field instance (any)\cr
    #'   During `instantiate()`, the instance is stored in this slot in an arbitrary format.
    #'   Note that if a grouping variable is present in the [Task], a [Resampling] may operate on the
    #'   group ids internally instead of the row ids (which may lead to confusion).
    #'
    #'   It is advised to not work directly with the `instance`, but instead only use the getters
    #'   `$train_set()` and `$test_set()`.
    instance = NULL,

    #' @field task_hash (`character(1)`)\cr
    #'   The hash of the [Task] which was passed to `r$instantiate()`.
    task_hash = NA_character_,

    #' @field task_nrow (`integer(1)`)\cr
    #'   The number of observations of the [Task] which was passed to `r$instantiate()`.
    #'
    task_nrow = NA_integer_,

    #' @field duplicated_ids (`logical(1)`)\cr
    #'   If `TRUE`, duplicated rows can occur within a single training set or within a single test set.
    #'   E.g., this is `TRUE` for Bootstrap, and `FALSE` for cross-validation.
    #'   Only used internally.
    duplicated_ids = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param duplicated_ids (`logical(1)`)\cr
    #'   Set to `TRUE` if this resampling strategy may have duplicated row ids in a single training set or test set.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [ResamplingCV] or [ResamplingHoldout].
    initialize = function(id, param_set = ps(), duplicated_ids = FALSE, label = NA_character_, man = NA_character_) {
      private$.id = assert_string(id, min.chars = 1L)
      self$label = assert_string(label, na.ok = TRUE)
      self$param_set = assert_param_set(param_set)
      self$duplicated_ids = assert_flag(duplicated_ids)
      self$man = assert_string(man, na.ok = TRUE)
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      catn(format(self), if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label))
      catn(str_indent("* Iterations:", self$iters))
      catn(str_indent("* Instantiated:", self$is_instantiated))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values, 1000L)))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Materializes fixed training and test splits for a given task and stores them in `r$instance`
    #' in an arbitrary format.
    #'
    #' @param task ([Task])\cr
    #'   Task used for instantiation.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    instantiate = function(task) {
      task = assert_task(as_task(task))
      strata = task$strata
      groups = task$groups

      if (is.null(strata)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids, task = task)
        } else {
          private$.groups = groups
          instance = private$.sample(unique(groups$group), task = task)
        }
      } else {
        instance = private$.combine(lapply(strata$row_id, private$.sample, task = task))
      }

      private$.hash = NULL
      self$instance = instance
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    },

    #' @description
    #' Returns the row ids of the i-th training set.
    #'
    #' @param i (`integer(1)`)\cr
    #'   Iteration.
    #' @return (`integer()`) of row ids.
    train_set = function(i) {
      private$.get_set(private$.get_train, i)
    },

    #' @description
    #' Returns the row ids of the i-th test set.
    #'
    #' @param i (`integer(1)`)\cr
    #'   Iteration.
    #' @return (`integer()`) of row ids.
    test_set = function(i) {
      private$.get_set(private$.get_test, i)
    }
  ),

  active = list(
    #' @template field_id
    id = function(rhs) {
      if (missing(rhs)) {
        return(private$.id)
      }

      private$.hash = NULL
      private$.id = assert_string(rhs, min.chars = 1L)
    },

    #' @field is_instantiated (`logical(1)`)\cr
    #'   Is `TRUE` if the resampling has been instantiated.
    is_instantiated = function(rhs) {
      assert_ro_binding(rhs)
      !is.null(self$instance)
    },

    #' @template field_hash
    hash = function(rhs) {
      assert_ro_binding(rhs)
      if (!self$is_instantiated) {
        return(NA_character_)
      }

      if (is.null(private$.hash)) {
        private$.hash = calculate_hash(list(class(self), self$id, self$param_set$values, self$instance))
      }

      private$.hash
    }
  ),

  private = list(
    .id = NULL,
    .hash = NULL,
    .groups = NULL,

    .get_set = function(getter, i) {
      if (!self$is_instantiated) {
        stopf("Resampling '%s' has not been instantiated yet", self$id)
      }
      i = assert_int(i, lower = 1L, upper = self$iters, coerce = TRUE)
      ids = getter(i)

      if (is.null(private$.groups)) {
        return(ids)
      }

      private$.groups[list(ids), on = "group", allow.cartesian = TRUE][[1L]]
    }
  )
)

ResamplingStrataGroupCV = R6Class(
  "ResamplingStrataGroupCV",
  inherit = ResamplingStrataGroup,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        folds = p_int(2L, tags = "required")
      )
      ps$values = list(folds = 10L)

      super$initialize(id = "cv", param_set = ps,
        label = "Cross-Validation", man = "mlr3::mlr_resamplings_cv")
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      as.integer(self$param_set$values$folds)
    }
  ),

  private = list(
    .sample = function(ids, ...) {
      data.table(
        row_id = ids,
        fold = shuffle(seq_along0(ids) %% as.integer(self$param_set$values$folds) + 1L),
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
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
