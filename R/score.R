score <- function(bench.result, ...){
  algorithm <- learner_id <- NULL
  ## Above to avoid CRAN NOTE.
  bench.score <- bench.result$score(...)
  out.dt.list <- list()
  for(score.i in 1:nrow(bench.score)){
    bench.row <- bench.score[score.i]
    it.dt <- bench.row$resampling[[1]]$instance$iteration.dt
    out.dt.list[[score.i]] <- it.dt[
      bench.row, on="iteration"
    ][, algorithm := sub(".*[.]", "", learner_id)]
  }
  out <- rbindlist(out.dt.list)
  class(out) <- c("score", class(out))
  out
}

plot.score <- function(x, ..., value.var=NULL){
  if(requireNamespace("animint2")){
    if(is.null(value.var)){
      value.var <- grep("classif|regr", names(x), value=TRUE)[1]
    }
    dt <- data.table(x)[, value := get(value.var)][]
    animint2::ggplot()+
      animint2::geom_point(animint2::aes(
        value, train.subsets),
        shape=1,
        data=dt)+
      animint2::facet_grid(
        algorithm ~ task_id + test.subset,
        labeller=label_both,
        scales="free")+
      animint2::scale_x_continuous(value.var)
  }
}  
