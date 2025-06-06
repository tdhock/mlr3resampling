add_algorithm <- function(DT){
  if(is.null(DT[["algorithm"]]) && !is.null(DT[["learner_id"]]))
    DT[, algorithm := sub(".*[.]", "", learner_id)]
}

score <- function(bench.result, ...){
  algorithm <- learner_id <- NULL
  ## Above to avoid CRAN NOTE.
  bench.score <- bench.result$score(...)
  out.dt.list <- list()
  for(score.i in 1:nrow(bench.score)){
    bench.row <- bench.score[score.i]
    it.dt <- bench.row$resampling[[1]]$instance$iteration.dt
    out.dt.list[[score.i]] <- it.dt[bench.row, on="iteration"]
  }
  out <- add_algorithm(rbindlist(out.dt.list))
  class(out) <- c("score", class(out))
  out
}

plot.score <- function(x, ..., value.var=NULL){
  value <- train.subsets <- NULL
  if(requireNamespace("ggplot2")){
    if(is.null(value.var)){
      value.var <- grep("classif|regr", names(x), value=TRUE)[1]
    }
    dt <- data.table(x)[, value := get(value.var)][]
    ggplot2::ggplot()+
      ggplot2::geom_point(ggplot2::aes(
        value, train.subsets),
        shape=1,
        data=dt)+
      ggplot2::facet_grid(
        algorithm ~ task_id + test.subset,
        labeller=ggplot2::label_both,
        scales="free")+
      ggplot2::scale_x_continuous(value.var)
  }
}  
