pvalue_compute <- function(
  score_value,
  panel_keys
){
  train.subsets <- same <- value <- value_mean <- value_sd <- . <- lo <- hi <- compare_mean <- same_mean <- hjust <- pmax_mean <- mid <- pmin_mean <- p.paired <- mid_lo <- mid_hi <- text_label <- text_value <- NULL
  missing_panel_keys <- setdiff(panel_keys, names(score_value))
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
  train_subset_levels <- canonical_levels[canonical_levels %in% present_levels]
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
    train_subset_levels
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
      is.na(p.paired), "P = NA",
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
      value_mean < mid_lo, 0,
      value_mean > mid_hi, 1,
      default=0.5)
  )][]
  list(stats=stats_range, pvalues=pval_range)
}

pvalue <- function(score_in, value.var=NULL, digits=3){
  Train_subsets <- train.subsets <- value <- value_mean <- value_sd <- . <- lo <- hi <- task_id <- algorithm <- test.subset <- same <- same_mean <- compare_mean <- hjust <- pmax_mean <- mid <- pmin_mean <- p.paired <- mid_lo <- mid_hi <- NULL
  if(is.null(value.var)){
    value.var <- grep("classif|regr", names(score_in), value=TRUE)[1]
    if(is.na(value.var)){
      stop("value.var=NULL which means to take the first column matching classif|regr, but there are none, so please pick one among: ", paste(names(score_in), collapse=", "))
    }
  }
  if(length(value.var) != 1){
    stop("value.var must be length=1")
  }
  if(!value.var %in% names(score_in)){
    stop("value.var must be a column name of score_in")
  }
  measure.possible <- c("other","all")
  measure.vars <- measure.possible[measure.possible %in% score_in$train.subsets]
  if(length(measure.vars)==0){
    stop("score_in$train.subsets does not contain 'all' or 'other' which are necessary for computing p-values")
  }
  levs.present <- c(
    "same",
    measure.vars,
    paste0(measure.vars,"-same"))
  levs.possible <- c(
    "all",
    "all-same",
    "same",
    "other-same",
    "other")
  levs <- levs.possible[levs.possible %in% levs.present]
  score_dt <- add_algorithm(data.table(score_in))[, let(
    Train_subsets = factor(train.subsets, levs),
    value = get(value.var)
  )]
  compute <- pvalue_compute(
    score_value=score_dt,
    panel_keys=c("task_id", "test.subset", "algorithm")
  )
  stats_range <- compute$stats
  stats_range[, text_label := sprintf(
    paste0("%.", digits, "f \u00B1 %.", digits, "f"),
    value_mean, value_sd
  )]
  structure(list(
    value.var=value.var,
    stats=stats_range,
    pvalues=compute$pvalues), class=c("pvalue", "list"))
}

plot.pvalue <- function(x, ..., text.size=5, p.color="grey50", sd.seg.size=1){
  value_mean <- Train_subsets <- hi <- lo <- compare_mean <- same_mean <- hjust <- text_label <- text_value <- label_both <- NULL
  if(requireNamespace("ggplot2")){
    ggplot2::ggplot()+
      ggplot2::theme_bw()+
      ggplot2::geom_point(ggplot2::aes(
        value_mean,
        Train_subsets),
        shape=1,
        data=x$stats)+
      ggplot2::geom_segment(ggplot2::aes(
        hi,
        Train_subsets,
        xend=lo, yend=Train_subsets),
        linewidth=sd.seg.size,
        data=x$stats)+
      ggplot2::geom_segment(ggplot2::aes(
        compare_mean, Train_subsets,
        xend=same_mean, yend=Train_subsets),
        color=p.color,
        data=x$pvalues)+
      ggplot2::geom_text(ggplot2::aes(
        value_mean,
        Train_subsets,
        hjust=hjust,
        label=text_label),
        size=text.size,
        vjust=-0.5,
        data=x$stats)+
      ggplot2::geom_text(ggplot2::aes(
        text_value, Train_subsets,
        label=text_label,
        hjust=hjust),
        color=p.color,
        size=text.size,
        vjust=-0.5,
        data=x$pvalues)+
      ggplot2::facet_grid(
        algorithm ~ task_id + test.subset,
        labeller=ggplot2::label_both,
        scales="free")+
      ggplot2::scale_x_continuous(
        x$value.var)+
      ggplot2::scale_y_discrete(
        "Train subsets",
        drop=FALSE)
  }
}

pvalue_downsample <- function(
  score_in,
  subset_name,
  model_name,
  value.var=NULL,
  digits=3
){
  Train_subsets <- train.subsets <- value <- value_mean <- value_sd <- . <- lo <- hi <- task_id <- algorithm <- test.subset <- same <- same_mean <- compare_mean <- hjust <- pmax_mean <- mid <- pmin_mean <- p.paired <- mid_lo <- mid_hi <- sample_size <- groups <- n.train.groups <- seed <- iteration <- NULL
  if(!is.character(subset_name) || length(subset_name) != 1L || is.na(subset_name)){
    stop("subset_name must be a non-NA character string of length 1")
  }
  if(!is.character(model_name) || length(model_name) != 1L || is.na(model_name)){
    stop("model_name must be a non-NA character string of length 1")
  }
  if(!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
     digits < 0L || as.integer(digits) != digits){
    stop("digits must be a non-negative integer scalar")
  }
  digits <- as.integer(digits)
  required.cols <- c(
    "test.subset", "train.subsets", "test.fold", "groups", "n.train.groups"
  )
  missing.cols <- setdiff(required.cols, names(score_in))
  if(length(missing.cols)){
    stop(
      "score_in is missing required columns: ",
      paste(missing.cols, collapse=", ")
    )
  }
  score_dt <- add_algorithm(data.table(score_in))
  if(!"algorithm" %in% names(score_dt)){
    stop("score_in must have either algorithm or learner_id column")
  }
  score_dt <- score_dt[test.subset == subset_name]
  if(nrow(score_dt) == 0L){
    stop("subset_name=\"", subset_name, "\" was not found in score_in$test.subset")
  }
  score_dt <- score_dt[algorithm == model_name]
  if(nrow(score_dt) == 0L){
    stop(
      "model_name=\"", model_name,
      "\" was not found in score_in$algorithm for subset_name=\"",
      subset_name, "\""
    )
  }
  if("task_id" %in% names(score_dt) && length(unique(score_dt$task_id)) != 1L){
    stop("score_in must contain exactly one task_id after filtering subset_name/model_name")
  }
  if(is.null(value.var)){
    value.candidates <- grep("classif|regr", names(score_dt), value=TRUE)
    if(length(value.candidates) != 1L){
      stop(
        "score_in must have exactly one metric column matching classif|regr, but found ",
        length(value.candidates), ": ",
        paste(value.candidates, collapse=", "),
        ". Please specify value.var explicitly."
      )
    }
    value.var <- value.candidates[[1]]
  }else{
    if(!is.character(value.var) || length(value.var) != 1L || is.na(value.var)){
      stop("value.var must be a non-NA character string of length 1")
    }
    if(!value.var %in% names(score_dt)){
      stop("value.var must be a column name of score_in after filtering subset_name/model_name")
    }
  }
  if(!any(score_dt$n.train.groups < score_dt$groups)){
    stop("scores do not have downsamples")
  }
  if(!"same" %in% score_dt$train.subsets){
    stop("score_in$train.subsets must contain 'same'")
  }
  measure.possible <- c("other", "all")
  measure.vars <- measure.possible[measure.possible %in% score_dt$train.subsets]
  if(length(measure.vars) == 0L){
    stop(
      "score_in$train.subsets must contain at least one comparison subset among: all, other"
    )
  }
  min.groups <- min(score_dt$groups)
  score.full <- score_dt[n.train.groups == groups]
  score.small <- score_dt[n.train.groups == min.groups]
  if(nrow(score.small) == 0L ||
     !"same" %in% score.small$train.subsets ||
     !any(measure.vars %in% score.small$train.subsets)){
    stop("scores do not have downsamples at common size min(groups)")
  }
  score_panels <- rbind(
    score.full[, sample_size := "full"],
    score.small[, sample_size := as.character(min.groups)],
    fill=TRUE
  )[, sample_size := factor(sample_size, c("full", as.character(min.groups)))]
  score_value <- score_panels[, let(
    Train_subsets = as.character(train.subsets),
    value = get(value.var)
  )]
  all.labels <- unique(c(score_value$Train_subsets, paste0(measure.vars, "-same")))
  label_order <- c("other", "other-same", "same", "all-same", "all")
  label_order <- label_order[label_order %in% all.labels]
  score_value[, Train_subsets := factor(Train_subsets, label_order)]
  compute <- pvalue_compute(
    score_value=score_value,
    panel_keys="sample_size"
  )
  stats_range <- compute$stats
  pval_range <- compute$pvalues
  base_label <- sprintf(
    paste0("%.", digits, "f \u00B1 %.", digits, "f"),
    stats_range$value_mean, stats_range$value_sd
  )
  if(all(c("sample_size", "n.train") %in% names(stats_range))){
    stats_range[, text_label := ifelse(
      sample_size == "full",
      paste0(base_label, ", N = ", n.train),
      base_label
    )]
  }else{
    stats_range[, text_label := base_label]
  }
  stats_range[, Train_subsets := factor(as.character(Train_subsets), label_order)]
  pval_range[, Train_subsets := factor(as.character(Train_subsets), label_order)]
  n.test.folds <- length(unique(score_dt$test.fold))
  structure(list(
    subset_name=subset_name,
    model_name=model_name,
    value.var=value.var,
    label_order=label_order,
    n.test.folds=n.test.folds,
    caption=sprintf(
      "%s (mean \u00B1 sd) | subset: %s | model: %s | %d test folds",
      value.var,
      subset_name,
      model_name,
      n.test.folds
    ),
    stats=stats_range,
    pvalues=pval_range), class=c("pvalue_downsample", "list"))
}

plot.pvalue_downsample <- function(
  x,
  ...,
  text.size=4,
  p.color="grey50",
  sd.seg.size=0.8
){
  value_mean <- Train_subsets <- hi <- lo <- compare_mean <- same_mean <- hjust <- text_label <- text_value <- NULL
  if(requireNamespace("ggplot2")){
    ggplot2::ggplot()+
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
        linewidth=sd.seg.size,
        data=x$stats)+
      ggplot2::geom_segment(ggplot2::aes(
        compare_mean, Train_subsets,
        xend=same_mean, yend=Train_subsets),
        color=p.color,
        data=x$pvalues)+
      ggplot2::geom_text(ggplot2::aes(
        value_mean,
        Train_subsets,
        hjust=hjust,
        label=text_label),
        size=text.size,
        vjust=-0.5,
        data=x$stats)+
      ggplot2::geom_text(ggplot2::aes(
        text_value, Train_subsets,
        label=text_label,
        hjust=hjust),
        color=p.color,
        size=text.size,
        vjust=-0.5,
        data=x$pvalues)+
      ggplot2::facet_grid(
        . ~ sample_size,
        labeller=ggplot2::labeller(
          sample_size=function(v)ifelse(
            v == "full",
            "sample_size: full",
            paste0("sample_size: smallest = ", v))),
        scales="free")+
      ggplot2::scale_x_continuous(
        NULL,
        labels=function(v)sprintf("%.3f", v))+
      ggplot2::scale_y_discrete(
        "Train subsets",
        drop=TRUE,
        limits=function(l)rev(x$label_order[x$label_order %in% l]))+
      ggplot2::labs(
        caption=x$caption
      )+
      ggplot2::theme(
        plot.caption=ggplot2::element_text(hjust=0.5, size=12)
      )
  }
}
