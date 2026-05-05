ResamplingSameOtherSizesCV = R6::R6Class(
  "ResamplingSameOtherSizesCV",
  inherit=ResamplingBase,
  public = list(
    initialize = function() {
      ps = paradox::ps(
        folds = paradox::p_int(2L, tags = "required"),
        seeds = paradox::p_int(1L, tags = "required"),
        ratio = paradox::p_dbl(0,1, tags = "required"),
        sizes = paradox::p_int(-1, tags = "required"),
        ignore_subset = paradox::p_lgl(tags="required"),
        subsets = paradox::p_fct(c("S","O","A","SO","SA","SOA"),tags="required"),
        group_stratum_algo=paradox::p_fct(c("Wasikowski","WasikowskiLinearMemory","RSS"),tags="required")
      )
      ps$values = list(
        folds=3L,
        seeds=1L,
        ratio=0.5,
        sizes=-1L,
        ignore_subset=FALSE,
        subsets="SOA",
        group_stratum_algo="RSS"
      )
      super$initialize(
        id = "same_other_sizes_cv",
        param_set = ps,
        label = "Compare Same/Other and Sizes Cross-Validation",
        man = "ResamplingSameOtherSizesCV")
    }
  ),
  private = list(
    .get_instance = function(task) {
      . <- train_groups <- test.subset <- same <- full <- other <- stratum <- group <- row_id <- fold <- groups <- prop <- iteration <- stratum_fac <- random_order <- neg_sd <- neg_nrow <- freq <- g_ord <- NULL
      ## Above to avoid CRAN NOTEs.
      reserved.names <- c(
        "row_id", "fold",
        "subset", "group",
        "display_row",
        "train.subsets", "test.fold", "test.subset", "iteration", 
        "test", "train", "algorithm", "uhash", "nr", "task", "task_id",
        "learner", "learner_id", "resampling", "resampling_id",
        "prediction")
      subset.vec <- task$col_roles[["subset"]]
      subset.dt <- data.table(
        test.subset=if(self$param_set$values$ignore_subset || length(subset.vec)==0){
          rep("full", task$nrow)
        }else{
          bad.names <- subset.vec[subset.vec %in% reserved.names]
          if(length(bad.names)){
            first.bad <- bad.names[1]
            stop(sprintf("col with role subset must not be named %s; please fix by renaming %s col", first.bad, first.bad))
          }
          task$data(cols=subset.vec)[[1]]
        }
      )
      n.subsets <- length(unique(subset.dt[["test.subset"]]))
      train.subsets <- if(n.subsets==1)"same" else switch(
        self$param_set$values$subsets,
        S="same",
        O="other",
        A="all",
        SO=c("same","other"),
        SA=c("same","all"),
        SOA=c("same","other","all"))
      n.folds <- self$param_set$values$folds
      acol <- task$col_roles$group
      avec <- if(length(acol)==1){
        task$data(cols=acol)[[acol]]
      }else{
        1:task$nrow
      }
      subset.groupic <- unique(data.table(subset.dt, group=avec))
      train.counts.wide <- subset.groupic[, .(
        full=.N
      ), by=test.subset][
      , same := as.integer(full*(n.folds-1)/n.folds)
      ][
      , all := sum(same)
      ][
      , other := all-same
      ][]
      train.counts.tall <- melt(
        train.counts.wide,
        measure.vars=train.subsets,
        id.vars="test.subset",
        variable.name="train.subsets",
        value.name="groups")
      strata.dt <- if(length(task$col_roles$stratum)){
        task$data(
          cols=task$col_roles$stratum
        )[, stratum := .GRP, by=c(task$col_roles$stratum)][]
      }else{
        data.table(stratum=rep(1L, task$nrow))
      }
      group.row.dt <- data.table(
        ## test.subset, stratum, group, row_id.
        subset.dt, strata.dt, group=avec, row_id=task$row_ids)
      fcol <- task$col_roles$fold
      fold.dt <- if(length(fcol)==1){
        fold <- task$data(cols=fcol)[[fcol]]
        if(length(acol)==1){
          group.fold.dt <- unique(data.table(group=avec, fold))
          if(any(group.fold.dt[, .N, by=group][["N"]] > 1L)){
            stop("task$col_roles$fold must be constant within each group")
          }
        }
        data.table(group.row.dt, fold)
      }else{
        scounts <- group.row.dt[, .(
          N=.N
        ), keyby=.(group,stratum)][, .(
          strata=.N
        ), by=group]
        if(any(scounts$strata>1)){
          ## less efficient code for fold assignment when there are
          ## some groups in multiple strata.
          if(grepl("Wasikowski", self$param_set$values$group_stratum_algo)){
            table_prop <- function(x){
              stab <- table(x)
              stab/sum(stab)
            }
            ptab <- group.row.dt[, let(
              random_order = sample(.N),
              stratum_fac = factor(stratum)
            )][, table_prop(stratum_fac)]
            group.row.dt[, let(
              neg_sd = -sd(table(stratum_fac)),
              g_ord = min(random_order)
            ), by=group]
            setkey(group.row.dt, neg_sd, g_ord)
            fun <- get(paste0(
              "stratified_group_cv_",
              self$param_set$values$group_stratum_algo,
              "_interface"))
            group.row.dt[
            , fold := fun(
              stratum-1L, cumsum(c(FALSE, diff(g_ord)!=0)), n.folds
            )+1L]
          }else{
            ideal.tab <- group.row.dt[, let(
              random_order = sample(.N),
              stratum_fac = factor(stratum)
            )][, table(stratum_fac)/n.folds]
            group.row.dt[, let(
              rss = sum((table(stratum_fac)-ideal.tab)^2),
              neg_nrow = -.N,
              freq = mean(ideal.tab*table(stratum_fac)),
              g_ord = min(random_order)
            ), by=group]
            setkey(group.row.dt, rss, neg_nrow, freq, g_ord)
            group.row.dt[
            , fold := stratified_group_cv_RSS_interface(
              stratum-1L, cumsum(c(FALSE, diff(g_ord)!=0)), n.folds
            )+1L]
          }
        }else{
          ## more efficient code for fold assignment when each group
          ## is in a different stratum.
          sample.dt <- group.row.dt[
            ## stratum, row_id, fold. (but row_id means group)
          , private$.sample(unique(group), task=task) #assigns fold.
          , by=stratum]
          sample.dt[, .(
            group=row_id, fold
          )][group.row.dt, on="group"]
        }
      }[order(row_id), .(group, fold, test.subset, stratum, row_id)]
      train.test.subset <- setkey(data.table(
        train.subsets
      )[
      , unique(data.table(
        test.fold=fold.dt$fold,
        subset.dt))
      , by=train.subsets
      ])
      train.size.dt <- train.counts.tall[, .(
        train_groups=unique(as.integer(sort(c(
          if(self$param_set$values$sizes>=1){
            min(groups)*self$param_set$values$ratio^seq(1, self$param_set$values$sizes)
          },
          groups))))
      ), by=test.subset]
      train.test.groups <- train.counts.tall[
        train.test.subset,
        on=c("train.subsets","test.subset")]
      iteration.dt.list <- list()
      for(tta.i in 1:nrow(train.test.groups)){
        tta.row <- train.test.groups[tta.i]
        op.chr <- if(self$param_set$values$sizes <= 0)"==" else ">="
        on.vec <- c("test.subset", paste("groups",op.chr,"train_groups"))
        n.train.groups.vec <- unique(c(
          tta.row[
          train.size.dt,
          groups,
          on=on.vec,
          nomatch=0L],
          if(self$param_set$values$sizes == 0) train.size.dt[
            tta.row, min(train_groups), on="test.subset"]
        ))
        for(seed in 1:self$param_set$values$seeds){
          is.set.subset <- list(
            test=fold.dt[["test.subset"]] == tta.row[["test.subset"]])
          is.set.subset[["train"]] <- switch(
            tta.row[["train.subsets"]],
            same=is.set.subset[["test"]],
            other=!is.set.subset[["test"]],
            all=rep(TRUE, length(is.set.subset[["test"]])))
          is.set.fold <- list(
            test=fold.dt[["fold"]] == tta.row[["test.fold"]])
          is.set.fold[["train"]] <- !is.set.fold[["test"]]
          fold.train.dt <- fold.dt[is.set.fold$train & is.set.subset$train]
          prop.train.dt <- fold.train.dt[, .(
            groups=length(unique(group))
          ), by=stratum][
          , prop := groups/sum(groups)
          ][]
          group.train.dt <- unique(fold.train.dt[, .(
            group, stratum
          )])[
            sample(.N)#random seed used here to order groups.
          ][
          , groups := max(n.train.groups.vec)*(seq_along(group)/length(group))
          , by=stratum
          ][order(stratum, groups)]
          for(n.train.groups in n.train.groups.vec){
            train_subset <- group.train.dt[groups <= n.train.groups, group]
            is.set.groups <- list(
              test=rep(TRUE, nrow(fold.dt)),
              train=fold.dt$group %in% train_subset)
            for(set.name in names(is.set.fold)){
              is.subset <- is.set.subset[[set.name]]
              is.fold <- is.set.fold[[set.name]]
              is.group <- is.set.groups[[set.name]]
              is.set.dt <- fold.dt[is.subset & is.fold & is.group]
              set(
                tta.row,
                j=set.name,
                value=list(is.set.dt[["row_id"]]))
            }
            iteration.dt.list[[paste(tta.i, seed, n.train.groups)]] <- data.table(
              tta.row, seed, n.train.groups)
          }
        }
      }
      list(
        iteration.dt=rbindlist(iteration.dt.list)[, let(
          iteration = .I,
          Train_subsets = factor(train.subsets, c("all","same","other"))
        )][],
        fold.dt=fold.dt,
        group.row.dt=group.row.dt)
    }
  )
)
