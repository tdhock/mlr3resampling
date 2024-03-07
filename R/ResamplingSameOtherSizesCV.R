ResamplingSameOtherSizesCV = R6::R6Class(
  "ResamplingSameOtherSizesCV",
  inherit=ResamplingBase,
  public = list(
    initialize = function() {
      ps = paradox::ps(
        folds = paradox::p_int(2L, tags = "required"),
        seeds = paradox::p_int(1L, tags = "required"),
        ratio = paradox::p_dbl(0,1, tags = "required"),
        sizes = paradox::p_int(0, tags = "required")
      )
      ps$values = list(folds = 3L, seeds=3L, ratio=0.5, sizes=2L)
      super$initialize(
        id = "same_other_sizes_cv",
        param_set = ps,
        label = "Compare Same/Other and Sizes Cross-Validation",
        man = "ResamplingSameOtherSizesCV")
    },
    instantiate = function(task) {
      task = mlr3::assert_task(mlr3::as_task(task))
      group.gen.vec <- task$col_roles[["group_generalization"]]
      if(length(group.gen.vec)==0){
        stop('task has no group_generalization, but at least one column with role group_generalization is required; use task$set_col_roles(group_col, c("group_generalization","stratum"))')
      }
      reserved.names <- c(
        "row_id", "fold",
        "group_generalization", "group_atomic",
        "display_row",
        "train.groups", "test.fold", "test.group", "iteration", 
        "test", "train", "algorithm", "uhash", "nr", "task", "task_id",
        "learner", "learner_id", "resampling", "resampling_id",
        "prediction")
      bad.names <- group.gen.vec[group.gen.vec %in% reserved.names]
      if(length(bad.names)){
        first.bad <- bad.names[1]
        stop(sprintf("col with role group_generalization must not be named %s; please fix by renaming %s col", first.bad, first.bad))
      }
      group.gen.dt <- data.table(
        test.group=task$data(cols=group.gen.vec)[[1]]
      )
      n.folds <- self$param_set$values$folds
      acol <- task$col_roles$group_atomic
      avec <- if(is.null(acol)){
        1:nrow(group.gen.dt)
      }else{
        task$data(cols=acol)[[acol]]
      }
      group.gen.atomic <- unique(data.table(group.gen.dt, group_atomic=avec))
      train.counts.wide <- group.gen.atomic[, .(
        full=.N
      ), by=test.group][
      , same := full*(n.folds-1)/n.folds
      ][
      , all := sum(same)
      ][
      , other := all-same
      ][]
      train.counts.tall <- melt(
        train.counts.wide,
        measure.vars=c("same","other","all"),
        id.vars="test.group",
        variable.name="train.groups",
        value.name="atoms")
      if(is.null(task$col_roles$stratum)){
        stop('task has no strata, but at least one stratum variable is required; at least assign the group variable to a stratum, task$set_col_roles(group_col, c("group","stratum"))')
      }
      strata.dt <- task$data(cols=task$col_roles$stratum)[, stratum := .GRP, by=c(task$col_roles$stratum)][]
      atom.row.dt <- data.table(
        group.gen.dt, strata.dt, group_atomic=avec, row_id=seq_along(avec))
      sample.dt <- atom.row.dt[
      , private$.sample(unique(group_atomic), task=task)
      , by=stratum]
      fold.dt <- sample.dt[, .(
        group_atomic=row_id, fold
      )][atom.row.dt, on="group_atomic"]
      train.test.group <- setkey(data.table(
        train.groups=c("all","other","same")
      )[
      , unique(data.table(
        test.fold=fold.dt$fold,
        group.gen.dt))
      , by=train.groups
      ])
      train.size.dt <- train.counts.tall[, .(
        train_atoms=unique(sort(c(
          min(atoms)*self$param_set$values$ratio^seq(1, self$param_set$values$sizes),
          atoms)))
      ), by=test.group]
      train.test.atoms <- train.counts.tall[train.test.group, on=c("train.groups","test.group")]
      iteration.dt.list <- list()
      for(tta.i in 1:nrow(train.test.atoms)){
        tta.row <- train.test.atoms[tta.i]
        n.train.atoms.vec <- tta.row[
          train.size.dt,
          atoms,
          on=.(test.group, atoms >= train_atoms),
          nomatch=0L]
        for(seed in 1:self$param_set$values$seeds){
          is.set.group <- list(test=fold.dt[["test.group"]] == tta.row[["test.group"]])
          is.set.group[["train"]] <- switch(
            tta.row[["train.groups"]],
            same=is.set.group[["test"]],
            other=!is.set.group[["test"]],
            all=rep(TRUE, length(is.set.group[["test"]])))
          is.set.fold <- list(
            test=fold.dt[["fold"]] == tta.row[["test.fold"]])
          is.set.fold[["train"]] <- !is.set.fold[["test"]]
          fold.train.dt <- fold.dt[is.set.fold$train & is.set.group$train]
          prop.train.dt <- fold.train.dt[, .(
            atoms=length(unique(group_atomic))
          ), by=stratum][
          , prop := atoms/sum(atoms)
          ][]
          atom.train.dt <- unique(fold.train.dt[, .(
            group_atomic, stratum
          )])[
            sample(.N)#random seed used here to order atoms.
          ][
          , atoms := max(n.train.atoms.vec)*seq_along(group_atomic)/length(group_atomic)
          , by=stratum
          ][order(stratum, atoms)]
          for(n.train.atoms in n.train.atoms.vec){
            train_group_atomic <- atom.train.dt[atoms <= n.train.atoms, group_atomic]
            is.set.atoms <- list(
              test=rep(TRUE, nrow(fold.dt)),
              train=fold.dt$group_atomic %in% train_group_atomic)
            for(set.name in names(is.set.fold)){
              is.group <- is.set.group[[set.name]]
              is.fold <- is.set.fold[[set.name]]
              is.atom <- is.set.atoms[[set.name]]
              is.set.dt <- fold.dt[is.group & is.fold & is.atom]
              set(
                tta.row,
                j=set.name,
                value=list(is.set.dt[["row_id"]]))
            }
            iteration.dt.list[[paste(tta.i, seed, n.train.atoms)]] <- data.table(
              tta.row, seed, n.train.atoms)
          }
        }
      }
      self$instance <- list(
        iteration.dt=rbindlist(iteration.dt.list))
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  )
)
