pvalue <- function(score_in, value.var=NULL, digits=3){
  Train_subsets <- train.subsets <- value <- value_mean <- value_sd <- . <- lo <- hi <- task_id <- algorithm <- test.subset <- same <- same_mean <- compare_mean <- hjust <- pmax_mean <- mid <- pmin_mean <- p.paired <- NULL
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
  levs <- c(
    levs.possible[levs.possible %in% levs.present],
    "")#for space above.
  score_dt <- data.table(score_in)[
  , Train_subsets := factor(train.subsets, levs)
  ][
  , value := get(value.var)
  ][]
  score_wide <- dcast(
    score_dt,
    task_id + test.subset + algorithm + test.fold ~ train.subsets)
  score_long <- melt(
    score_wide,
    measure.vars=measure.vars,
    variable.name="train.subsets")
  stats_dt <- dcast(
    score_dt,
    task_id + test.subset + algorithm + Train_subsets ~ .,
    list(mean, sd, length)
  )[, let(
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
      max_val)
  }, by=.(task_id, test.subset)]
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
      N=.N)
  }, by=.(
    task_id, test.subset, algorithm,
    Train_subsets=factor(paste0(train.subsets,"-same"), levs)
  )]
  pval_range <- range_dt[
    pval_dt, on=.(task_id,test.subset)
  ][, let(
    pmin_mean = pmin(same_mean, compare_mean),
    pmax_mean = pmax(same_mean, compare_mean)
  )][
  , hjust := fcase(
    pmax_mean < mid, 0,
    pmin_mean > mid, 1,
    default=0.5)
  ][, let(
    text_label = paste0(
      ifelse(
        p.paired<0.0001,
        "P<0.0001",
        sprintf("P=%.4f", p.paired))),
    text_value = fcase(
      hjust==0, pmin_mean,
      hjust==1, pmax_mean,
      default=(pmin_mean+pmax_mean)/2)
  )][]
  stats_range <- range_dt[
    stats_dt, on=.(task_id,test.subset)
  ][, let(
    hjust = fcase(
      value_mean<mid_lo, 0,
      value_mean>mid_hi, 1,
      default=0.5),
    text_label = sprintf(
      paste0("%.",digits,"f\u00B1%.",digits,"f"),
      value_mean, value_sd)
  )][]
  structure(list(
    value.var=value.var,
    stats=stats_range,
    pvalues=pval_range), class=c("pvalue", "list"))
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
        size=sd.seg.size,
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
