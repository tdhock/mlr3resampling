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
  rbindlist(out.dt.list)
}
