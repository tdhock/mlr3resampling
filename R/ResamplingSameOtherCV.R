ResamplingSameOtherCV = R6::R6Class(
  "ResamplingSameOtherCV",
  inherit=ResamplingBase,
  public = list(
    initialize = function() {
      ps = paradox::ps(
        folds = paradox::p_int(2L, tags = "required")
      )
      ps$values = list(folds = 3L)
      super$initialize(
        id = "same_other_cv",
        param_set = ps,
        label = "Same versus Other Cross-Validation",
        man = "ResamplingSameOtherCV")
    },
    get_instance = function(task) {
      row_id <- fold <- display_row <- . <- train.subsets <- iteration <- NULL
      ## Above to avoid CRAN NOTEs.
      if(length(task$col_roles$group)){
        stop("since version 2024.4.15, ResamplingSameOtherCV no longer supports group role (used to avoid splitting related rows into different subsets), but still supports defining same/other/all train groups (now called subset role). Please fix by either changing group role to subset, or using ResamplingSameOtherSizesCV instead (it supports group and subset).")
      }
      subset.name.vec <- task$col_roles$subset
      if(length(subset.name.vec)==0){
        stop('task has no subset, but at least one subset variable is required')
      }
      reserved.names <- c(
        "row_id", "fold", "subset", "display_row",
        "train.subsets", "test.fold", "test.subset", "iteration",
        "test", "train", "algorithm", "uhash", "nr", "task", "task_id",
        "learner", "learner_id", "resampling", "resampling_id",
        "prediction")
      bad.names <- subset.name.vec[subset.name.vec %in% reserved.names]
      if(length(bad.names)){
        first.bad <- bad.names[1]
        stop(sprintf("col with role subset must not be named %s; please fix by renaming %s col", first.bad, first.bad))
      }
      orig.subset.dt <- task$data(
        cols=subset.name.vec
      )[, subset := .GRP, by=subset.name.vec][]
      if(is.null(task$strata)){
        stop('task has no strata, but at least one stratum variable is required; at least assign the subset variable to a stratum')
      }
      folds = rbindlist(lapply(task$strata$row_id, private$.sample, task = task))
      setkey(folds, row_id)
      id.fold.subsets <- data.table(
        folds,
        orig.subset.dt
      )[
        order(subset, fold)
      ][
      , display_row := .I
      ][]
      uniq.fold.subsets <- setkey(unique(data.table(
        id.fold.subsets[, .(test.fold=fold, test.subset=subset)],
        id.fold.subsets[, subset.name.vec, with=FALSE])))
      iteration.dt <- data.table(
        train.subsets=c("all","other","same")
      )[
      , data.table(uniq.fold.subsets)
      , by=train.subsets
      ][, iteration := .I]
      disp.dt.list <- list()
      for(iteration.i in 1:nrow(iteration.dt)){
        split.info <- iteration.dt[iteration.i]
        is.set.subset <- list(
          test=id.fold.subsets[["subset"]] == split.info[["test.subset"]])
        is.set.subset[["train"]] <- switch(
          split.info[["train.subsets"]],
          same=is.set.subset[["test"]],
          other=!is.set.subset[["test"]],
          all=rep(TRUE, nrow(id.fold.subsets)))
        is.set.fold <- list(
          test=id.fold.subsets[["fold"]] == split.info[["test.fold"]])
        is.set.fold[["train"]] <- !is.set.fold[["test"]]
        for(set.name in names(is.set.fold)){
          is.subset <- is.set.subset[[set.name]]
          is.fold <- is.set.fold[[set.name]]
          is.set.dt <- id.fold.subsets[is.subset & is.fold]
          mid.end.i <- is.set.dt[, which(c(diff(display_row),NA)!=1)]
          start.i <- c(1,mid.end.i+1)
          disp.dt.list[[paste(
            iteration.i, set.name
          )]] <- data.table(
            split.info[, .(
              iteration, train.subsets
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
        id.fold.subsets[, .(
          rows="subset",
          display_row=min(display_row),
          display_end=max(display_row)
        ), by=subset][, fold := NA],
        id.fold.subsets[, .(
          rows="fold",
          display_row=min(display_row),
          display_end=max(display_row)
        ), by=.(subset, fold)])
      list(
        iteration.dt=iteration.dt,
        id.dt=id.fold.subsets[order(row_id)],
        viz.set.dt=rbindlist(disp.dt.list),
        viz.rect.dt=viz.rect.dt)
    }
  )
)
