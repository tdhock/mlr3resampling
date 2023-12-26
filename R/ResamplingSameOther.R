ResamplingSameOther = R6::R6Class(
  "ResamplingSameOther",
  inherit=ResamplingBase,
  public = list(
    instantiate = function(task) {
      task = mlr3::assert_task(mlr3::as_task(task))
      group.name.vec <- task$col_roles$group
      if(length(group.name.vec)==0){
        stop('task has no group, but at least one group variable is required; use task$set_col_roles(group_col, c("group","stratum"))')
      }
      reserved.names <- c(
        "row_id", "fold", "group", "display_row",
        "train.groups", "test.fold", "test.group", "iteration", 
        "test", "train", "algorithm", "uhash", "nr", "task", "task_id",
        "learner", "learner_id", "resampling", "resampling_id",
        "prediction")
      bad.names <- group.name.vec[group.name.vec %in% reserved.names]
      if(length(bad.names)){
        first.bad <- bad.names[1]
        stop(sprintf("col with role group must not be named %s; please fix by renaming %s col", first.bad, first.bad))
      }
      orig.group.dt <- task$data(cols=group.name.vec)
      if(is.null(task$strata)){
        stop('task has no strata, but at least one stratum variable is required; at least assign the group variable to a stratum, task$set_col_roles(group_col, c("group","stratum"))')
      }
      folds = private$.combine(lapply(task$strata$row_id, private$.sample, task = task))
      id.fold.groups <- data.table(
        folds[task$groups, on="row_id"],
        orig.group.dt
      )[
        order(group, fold)
      ][
      , display_row := .I
      ][]
      uniq.fold.groups <- setkey(unique(data.table(
        id.fold.groups[, .(test.fold=fold, test.group=group)],
        id.fold.groups[, group.name.vec, with=FALSE])))
      iteration.dt <- data.table(
        train.groups=c("all","other","same")
      )[
      , data.table(uniq.fold.groups)
      , by=train.groups
      ][, iteration := .I]
      disp.dt.list <- list()
      for(iteration.i in 1:nrow(iteration.dt)){
        split.info <- iteration.dt[iteration.i]
        is.set.group <- list(
          test=id.fold.groups[["group"]] == split.info[["test.group"]])
        is.set.group[["train"]] <- switch(
          split.info[["train.groups"]],
          same=is.set.group[["test"]],
          other=!is.set.group[["test"]],
          all=rep(TRUE, nrow(id.fold.groups)))
        is.set.fold <- list(
          test=id.fold.groups[["fold"]] == split.info[["test.fold"]])
        is.set.fold[["train"]] <- !is.set.fold[["test"]]
        for(set.name in names(is.set.fold)){
          is.group <- is.set.group[[set.name]]
          is.fold <- is.set.fold[[set.name]]
          is.set.dt <- id.fold.groups[is.group & is.fold]
          mid.end.i <- is.set.dt[, which(c(diff(display_row),NA)!=1)]
          start.i <- c(1,mid.end.i+1)
          disp.dt.list[[paste(
            iteration.i, set.name
          )]] <- data.table(
            split.info[, .(
              iteration, train.groups
            )],
            is.set.dt[, .(
              set.name,
              is.set.dt[start.i],
              display_end=display_row[c(mid.end.i,.N)]
            )]
          )
          set(
            iteration.dt,
            i=iteration.i,
            j=set.name,
            value=list(is.set.dt[["row_id"]]))
        }
      }
      viz.rect.dt <- rbind(
        id.fold.groups[, .(
          rows="group",
          display_row=min(display_row),
          display_end=max(display_row)
        ), by=group][, fold := NA],
        id.fold.groups[, .(
          rows="fold",
          display_row=min(display_row),
          display_end=max(display_row)
        ), by=.(group, fold)])        
      self$instance <- list(
        iteration.dt=iteration.dt,
        id.dt=id.fold.groups[order(row_id)],
        viz.set.dt=rbindlist(disp.dt.list),
        viz.rect.dt=viz.rect.dt)
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  )
)

ResamplingSameOtherCV = R6::R6Class(
  "ResamplingSameOtherCV",
  inherit = ResamplingSameOther,
  public = list(
    initialize = function() {
      ps = paradox::ps(
        folds = paradox::p_int(2L, tags = "required")
      )
      ps$values = list(folds = 3L)
      super$initialize(
        id = "same_other_cv",
        param_set = ps,
        label = "Cross-Validation",
        man = "ResamplingSameOtherCV")
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
