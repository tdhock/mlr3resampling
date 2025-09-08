batch_sampler_stratified <- function(min_samples_per_stratum, shuffle=TRUE){
  torch::sampler(
    "StratifiedSampler",
    initialize = function(data_source) {
      self$data_source <- data_source
      TSK <- data_source$task
      self$stratum <- TSK$col_roles$stratum
      if(length(self$stratum)==0)stop(TSK$id, "task missing stratum column role")
      self$stratum_dt <- data.table(
        TSK$data(cols=self$stratum),
        row.id=1:TSK$nrow)
      self$set_batch_list()
    },
    set_batch_list = function() {
      get_indices <- if(shuffle){
        function(n)torch::as_array(torch::torch_randperm(n))+1L
      }else{
        function(n)1:n
      }
      index_dt <- self$stratum_dt[
        get_indices(.N)
      ][
      , i.in.stratum := 1:.N, by=c(self$stratum)
      ][]
      count_dt <- index_dt[, .(
        max.i=max(i.in.stratum)
      ), by=c(self$stratum)][order(max.i)]
      count_min <- count_dt$max.i[1]
      num_batches <- max(1, count_min %/% min_samples_per_stratum)
      max_samp <- num_batches * min_samples_per_stratum
      index_dt[
      , n.samp := i.in.stratum/max(i.in.stratum)*max_samp
      , by=c(self$stratum)
      ][
      , batch.i := ceiling(n.samp/min_samples_per_stratum)
      ][]
      self$batch_list <- split(index_dt$row.id, index_dt$batch.i)
      self$batch_sizes <- sapply(self$batch_list, length)
      self$batch_size_tab <- sort(table(self$batch_sizes))
      self$batch_size <- as.integer(names(self$batch_size_tab)[length(self$batch_size_tab)])
    },
    .iter = function() {
      batch.i <- 0
      function() {
        if (batch.i < length(self$batch_list)) {
          batch.i <<- batch.i + 1L
          indices <- self$batch_list[[batch.i]]
          if (batch.i == length(self$batch_list)) {
            self$set_batch_list()
          }
          return(indices)
        }
        coro::exhausted()
      }
    },
    .length = function() {
      length(self$batch_list)
    }
  )
}
