library(data.table)
test.list <- atime::atime_test_list(
  ## Test case derived from https://github.com/tdhock/stratified-group-cv#20-sept-2026
  ## https://github.com/tdhock/mlr3resampling/pull/105
  "Improved stats speed in #105"=atime::atime_test(
    N=2^seq(3, 20),
    setup={
      y <- rep(1:2, length.out=N)
      set.seed(1)
      groupID <- sample(rep(seq(1, N/2), length.out=N))
      N_dt <- data.table(y, groupID)
      train_task <- mlr3::as_task_classif(N_dt, target="y")
      train_task$col_roles$stratum <- "y"
      train_task$col_roles$group <- "groupID"
    },
    seconds.limit=1,
    expr={
      cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
      cv$param_set$values$folds <- 2
      cv$param_set$values$group_stratum_algo <- "RSS"
      cv$instantiate(train_task)
    },
    Fast="b5a1aa99eaaaa9f8d402ecbda1ac95b07921b96f", #merge commit of Fast PR.
    Slow="38b0c2ab61f091a0506eccffa5abd8954621c8d6" #parent of first commit (https://github.com/tdhock/mlr3resampling/commit/a4d684612eeecfe2291b4ef01ef13ed194127c19) in Fast PR.
  )
)
