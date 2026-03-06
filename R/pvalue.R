pvalue_prepare <- function(
  score_in,
  value.var=NULL,
  digits=3
){
  train.subsets <- value <- Train_subsets <- algorithm <- learner_id <- NULL
  if(!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
     digits < 0L || as.integer(digits) != digits){
    stop("digits must be a non-negative integer scalar")
  }
  digits <- as.integer(digits)
  score_dt <- add_algorithm(data.table(score_in))
  if(!"algorithm" %in% names(score_dt)){
    stop("score_in must have either algorithm or learner_id column")
  }
  if(nrow(score_dt) == 0L){
    stop("score_in must have at least one row")
  }
  if(is.null(value.var)){
    value.candidates <- grep("classif|regr", names(score_dt), value=TRUE)
    value.var <- value.candidates[1]
    if(is.na(value.var)){
      stop(
        "value.var=NULL which means to take the first column matching classif|regr, but there are none, so please pick one among: ",
        paste(names(score_in), collapse=", ")
      )
    }
  }else{
    if(!is.character(value.var) || length(value.var) != 1L || is.na(value.var)){
      stop("value.var must be a non-NA character string of length 1")
    }
    if(!value.var %in% names(score_dt)){
      stop("value.var must be a column name of score_in")
    }
  }
  measure.vars <- c("other", "all")[c("other", "all") %in% score_dt$train.subsets]
  if(length(measure.vars) == 0L){
    stop("score_in$train.subsets does not contain 'all' or 'other' which are necessary for computing p-values")
  }
  score_value <- score_dt[, let(
    Train_subsets=as.character(train.subsets),
    value=get(value.var)
  )]
  list(
    score_dt=score_dt,
    score_value=score_value,
    value.var=value.var,
    digits=digits,
    measure.vars=measure.vars
  )
}

pvalue_compute <- function(
  score_value,
  panel_keys,
  digits=3
){
  train.subsets <- same <- value <- value_mean <- value_sd <- . <- lo <- hi <- compare_mean <- same_mean <- hjust <- pmax_mean <- mid <- pmin_mean <- p.paired <- mid_lo <- mid_hi <- text_label <- text_value <- NULL
  cast_id_cols <- c(panel_keys, "test.fold", intersect("seed", names(score_value)))
  stats_by <- c(panel_keys, "Train_subsets")
  range_by <- setdiff(panel_keys, "algorithm")
  if(length(range_by) == 0L){
    range_by <- panel_keys
  }
  measure.vars <- intersect(c("other", "all"), unique(score_value$train.subsets))
  canonical_levels <- c("other", "other-same", "same", "all-same", "all")
  present_levels <- unique(c(
    as.character(score_value$Train_subsets),
    paste0(measure.vars, "-same")
  ))
  label_order <- canonical_levels[canonical_levels %in% present_levels]
  score_value[, Train_subsets := factor(as.character(Train_subsets), label_order)]
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
  ), by=stats_by]
  if("n.train.groups" %in% names(score_value)){
    n.train <- NULL
    n.train.dt <- score_value[, .(
      n.train=round(mean(n.train.groups))
    ), by=stats_by]
    stats_dt[n.train.dt, on=stats_by, n.train := i.n.train]
  }
  stats_dt <- stats_dt[, let(
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
  stats_range[, Train_subsets := factor(as.character(Train_subsets), label_order)]
  pval_range[, Train_subsets := factor(as.character(Train_subsets), label_order)]
  list(stats=stats_range, pvalues=pval_range, label_order=label_order)
}

pvalue <- function(score_in, value.var=NULL, digits=3){
  prep <- pvalue_prepare(
    score_in=score_in,
    value.var=value.var,
    digits=digits
  )
  compute <- pvalue_compute(
    score_value=prep$score_value,
    panel_keys=c("task_id", "test.subset", "algorithm"),
    digits=prep$digits
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
  task_id <- algorithm <- test.subset <- sample_size <- groups <- n.train.groups <- train.subsets <- text_label <- n.train <- NULL
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
  key_cols <- intersect(c("task_id", "test.subset", "algorithm"), names(prep$score_dt))
  score_dt <- prep$score_dt[
    prep$score_dt[1L, ..key_cols],
    on=key_cols,
    nomatch=0L
  ]
  if(!any(score_dt$n.train.groups < score_dt$groups)){
    stop("scores do not have downsamples")
  }
  score_panels <- rbind(
    copy(score_dt[n.train.groups == groups])[, sample_size := "full"],
    copy(score_dt[n.train.groups == min(score_dt$groups)])[, sample_size := as.character(min(score_dt$groups))],
    fill=TRUE
  )[, sample_size := factor(sample_size, c("full", as.character(min(score_dt$groups))))]
  
  compute <- pvalue_compute(
    score_value=score_panels[, let(
      Train_subsets = as.character(train.subsets),
      value = get(prep$value.var)
    )],
    panel_keys="sample_size",
    digits=prep$digits
  )
  compute$stats[sample_size == "full", text_label := paste0(text_label, ", N = ", n.train)]
  structure(list(
    subset_name=as.character(score_dt$test.subset[[1]]),
    model_name=as.character(score_dt$algorithm[[1]]),
    value.var=prep$value.var,
    label_order=compute$label_order,
    n.test.folds=length(unique(score_dt$test.fold)),
    caption=sprintf(
      "%s (mean \u00B1 sd) | subset: %s | model: %s | %d test folds",
      prep$value.var,
      as.character(score_dt$test.subset[[1]]),
      as.character(score_dt$algorithm[[1]]),
      length(unique(score_dt$test.fold))
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
