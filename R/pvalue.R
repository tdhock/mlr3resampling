pvalue_prepare <- function(
  score_in,
  value.var=NULL,
  digits
){
  train.subsets <- value <- Train_subsets <- algorithm <- NULL
  if(!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
     digits < 0L || digits != floor(digits)){
    stop("digits must be a non-negative integer scalar")
  }
  score_dt <- add_algorithm(data.table(score_in))
  if(!"algorithm" %in% names(score_dt)){
    stop("score_in must have either algorithm or learner_id column")
  }
  if(nrow(score_dt) == 0L){
    stop("score_in must have at least one row")
  }
  value.var <- if(is.null(value.var)) grep("classif|regr", names(score_dt), value=TRUE)[1] else value.var
  if(!is.character(value.var) || length(value.var) != 1L || is.na(value.var) || !value.var %in% names(score_dt)){
    stop("value.var must be a single existing metric column (or NULL to auto-select first classif|regr column)")
  }
  measure.vars <- intersect(c("other", "all"), unique(score_dt$train.subsets))
  if(!length(measure.vars)){
    stop("score_in$train.subsets must contain at least one of: all, other")
  }
  list(
    score_dt=score_dt[, let(
      Train_subsets=train.subsets,
      value=get(value.var)
    )],
    value.var=value.var,
    measure.vars=measure.vars
  )
}

pvalue_compute <- function(
  score_value,
  panel_keys,
  digits=3,
  downsample=FALSE
){
  train.subsets <- same <- value <- value_mean <- value_sd <- . <- lo <- hi <- compare_mean <- same_mean <- hjust <- pmax_mean <- mid <- pmin_mean <- p.paired <- mid_lo <- mid_hi <- text_label <- text_value <- Train_subsets <- n.train.groups <- NULL
  cast_id_cols <- c(panel_keys, "test.fold", intersect("seed", names(score_value)))
  stats_by <- c(panel_keys, "Train_subsets")
  range_by <- setdiff(panel_keys, "algorithm")
  if(length(range_by) == 0L){
    range_by <- panel_keys
  }
  measure.vars <- intersect(c("other", "all"), unique(score_value$train.subsets))
  canonical_levels <- c("all", "all-same", "same", "other-same", "other")
  if(downsample)canonical_levels <- rev(canonical_levels)
  present_levels <- unique(c(
    score_value$Train_subsets,
    paste0(measure.vars, "-same")
  ))
  label_order <- canonical_levels[canonical_levels %in% present_levels]
  score_value[, Train_subsets := factor(Train_subsets, label_order)]
  score_wide <- dcast(
    score_value,
    formula=stats::as.formula(paste(
      paste(cast_id_cols, collapse=" + "),
      "~ train.subsets"
    )),
    value.var="value"
  )
  score_long <- melt(
    score_wide,
    id.vars=c(cast_id_cols, "same"),
    measure.vars=measure.vars,
    variable.name="train.subsets"
  )[, Train_subsets := factor(
    paste0(train.subsets, "-same"),
    label_order
  )][]
  stats_dt <- score_value[, .(
    value_mean=mean(value),
    value_sd=sd(value),
    value_length=.N
  ), by=c(stats_by, "n.train.groups")][, let(
    lo=value_mean-value_sd,
    hi=value_mean+value_sd
  )]
  range_dt <- stats_dt[, {
    min_val <- min(lo,na.rm=TRUE)
    max_val <- max(hi,na.rm=TRUE)
    data.table(
      min_val,
      mid_lo=min_val*2/3+max_val*1/3,
      mid=(min_val+max_val)/2,
      mid_hi=min_val*1/3+max_val*2/3,
      max_val
    )
  }, by=range_by]
  try.test <- function(...)tryCatch({
    t.test(...)
  }, error=function(e)list(estimate=NA_real_, p.value=NA_real_))
  pval_dt <- score_long[, {
    paired <- try.test(value, same, paired=TRUE)
    unpaired <- try.test(value, same, paired=FALSE)
    data.table(
      mean_diff=paired$estimate,
      diff_mean=diff(unpaired$estimate),
      p.paired=paired$p.value,
      p.unpaired=unpaired$p.value,
      same_mean=mean(same),
      compare_mean=mean(value),
      N=.N
    )
  }, by=stats_by]
  pval_range <- range_dt[
    pval_dt, on=range_by
  ][, let(
    pmin_mean = pmin(same_mean, compare_mean),
    pmax_mean = pmax(same_mean, compare_mean)
  )][
  , hjust := fcase(
    pmax_mean < mid, 0,
    pmin_mean > mid, 1,
    default=0.5)
  ][, let(
    text_label = fcase(
      p.paired < 0.0001, "P < 0.0001",
      default=sprintf("P = %.4f", p.paired)
    ),
    text_value = fcase(
      hjust==0, pmin_mean,
      hjust==1, pmax_mean,
      default=(pmin_mean+pmax_mean)/2)
  )][]
  stats_range <- range_dt[
    stats_dt, on=range_by
  ][, let(
    hjust = fcase(
      value_mean<mid_lo, 0,
      value_mean>mid_hi, 1,
      default=0.5)
  )][]
  stats_range[, text_label := sprintf(
    paste0("%.", digits, "f \u00B1 %.", digits, "f"),
    value_mean, value_sd
  )]
  stats_range[, Train_subsets := factor(Train_subsets, label_order)]
  pval_range[, Train_subsets := factor(Train_subsets, label_order)]
  list(stats=stats_range, pvalues=pval_range, label_order=label_order)
}

pvalue <- function(score_in, value.var=NULL, digits=3){
  n.train.groups <- groups <- NULL
  if(all(c("groups", "n.train.groups") %in% names(score_in))){
    score_in <- score_in[n.train.groups == groups]
  }
  prep <- pvalue_prepare(
    score_in=score_in,
    value.var=value.var,
    digits=digits
  )
  compute <- pvalue_compute(
    score_value=prep$score_dt,
    panel_keys=c("task_id", "test.subset", "algorithm"),
    digits=digits
  )
  structure(list(
    value.var=prep$value.var,
    stats=compute$stats,
    pvalues=compute$pvalues), class=c("pvalue", "list"))
}

pvalue_ggplot <- function(x){
  value_mean <- Train_subsets <- hi <- lo <- compare_mean <- same_mean <- hjust <- text_label <- text_value <- NULL
  if(requireNamespace("ggplot2")){
    out.gg <- ggplot2::ggplot()+
      ggplot2::theme_bw()+
      ggplot2::geom_point(ggplot2::aes(
        value_mean,
        Train_subsets),
        shape=1,
        size=1.8,
        data=x$stats)+
      ggplot2::geom_segment(ggplot2::aes(
        hi,
        Train_subsets,
        xend=lo, yend=Train_subsets),
        linewidth=0.8,
        data=x$stats)+
      ggplot2::geom_segment(ggplot2::aes(
        compare_mean, Train_subsets,
        xend=same_mean, yend=Train_subsets),
        color="grey50",
        data=x$pvalues)+
      ggplot2::geom_text(ggplot2::aes(
        value_mean,
        Train_subsets,
        hjust=hjust,
        label=text_label),
        size=4,
        vjust=-0.5,
        data=x$stats)+
      ggplot2::geom_text(ggplot2::aes(
        text_value, Train_subsets,
        label=text_label,
        hjust=hjust),
        color="grey50",
        size=4,
        vjust=-0.5,
        data=x$pvalues)
    out.gg
  }
}

plot.pvalue <- function(x, ...){
  pvalue_ggplot(x)+
    ggplot2::facet_grid(
      algorithm ~ task_id + test.subset,
      labeller=ggplot2::label_both,
      scales="free")+
    ggplot2::scale_x_continuous(
      paste0(x$value.var, " (mean \u00B1 sd)"))+
    ggplot2::scale_y_discrete(
      "Train subsets",
      drop=FALSE)
}

pvalue_downsample <- function(
  score_in,
  value.var=NULL,
  digits=3
){
  task_id <- algorithm <- test.subset <- sample_size <- groups <- n.train.groups <- train.subsets <- text_label <- NULL
  prep <- pvalue_prepare(
    score_in=score_in,
    value.var=value.var,
    digits=digits
  )
  required.cols <- c(
    "test.subset", "test.fold", "groups", "n.train.groups"
  )
  missing.cols <- setdiff(required.cols, names(prep$score_dt))
  if(length(missing.cols)){
    stop(
      "score_in is missing required columns: ",
      paste(missing.cols, collapse=", ")
    )
  }
  ss_levs <- c("full", min(prep$score_dt[["groups"]]))
  score_panels <- data.table(sample_size=factor(ss_levs, ss_levs))[
  , prep$score_dt[n.train.groups == if(sample_size=="full")groups else min(groups)]
  , by=sample_size]
  compute <- pvalue_compute(
    score_value=score_panels[, let(
      Train_subsets = train.subsets,
      value = get(prep$value.var)
    )],
    panel_keys="sample_size",
    digits=digits,
    downsample=TRUE
  )
  compute$stats[sample_size == "full", text_label := paste0(text_label, ", N = ", n.train.groups)]
  structure(list(
    value.var=prep$value.var,
    label_order=compute$label_order,
    caption=sprintf(
      "%s (mean \u00B1 sd) | subset: %s | model: %s | %d test folds",
      prep$value.var,
      score_in$test.subset[[1]],
      score_in$algorithm[[1]],
      length(unique(score_in$test.fold))
    ),
    stats=compute$stats,
    pvalues=compute$pvalues), class=c("pvalue_downsample", "list"))
}

plot.pvalue_downsample <- function(x, ...){
  pvalue_ggplot(x)+
    ggplot2::facet_grid(
      . ~ sample_size,
      labeller=ggplot2::labeller(
        sample_size=function(v)ifelse(
          v == "full",
          "sample_size: full",
          paste0("sample_size: smallest = ", v))),
      scales="free")+
    ggplot2::scale_x_continuous(
      x$caption)+
    ggplot2::scale_y_discrete(
      "Train subsets",
      drop=TRUE,
      limits=function(l)rev(x$label_order[x$label_order %in% l]))
}
