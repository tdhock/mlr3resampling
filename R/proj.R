proj_grid <- function(proj_dir, tasks, learners, resamplings, order_jobs=NULL, score_args=NULL, save_learner=FALSE, save_pred=FALSE){
  proj.grid <- list()
  for(arg in c("tasks", "learners", "resamplings")){
    value <- get(arg)
    if(!is.list(value)){
      value <- list(value)
    }
    proj.grid[[arg]] <- value
  }
  do.call(mlr3::benchmark_grid, proj.grid) # error checking.
  proj.grid$score_args <- score_args
  for(fun_name in paste0("save_",c("learner","pred"))){
    fun <- get(fun_name)
    fun <- if(identical(fun, FALSE)){
      function(L)NULL
    }else if(identical(fun, TRUE)){
      function(L)L
    }else if(!is.function(fun))stop(fun_name, " should be a function")
    proj.grid[[fun_name]] <- fun
  }
  dir.create(proj_dir, showWarnings = FALSE)
  saveRDS(proj.grid, file.path(proj_dir, "grid.rds"))
  ml_job_dt_list <- list()
  for(resampling.i in seq_along(proj.grid$resamplings)){
    resampling.obj <- proj.grid$resamplings[[resampling.i]]
    for(task.i in seq_along(proj.grid$tasks)){
      task.obj <- proj.grid$tasks[[task.i]]
      resampling.obj$instantiate(task.obj)
      iteration <- resampling.obj$instance$iteration.dt
      if(is.null(iteration)){
        iteration <- seq_len(resampling.obj$iters)
      }
      for(learner.i in seq_along(proj.grid$learners)){
        ml_job_dt_list[[paste(task.i, learner.i, resampling.i)]] <- data.table(
          task.i, learner.i, resampling.i,
          task.id=task.obj$id,
          learner.id=proj.grid$learners[[learner.i]]$id,
          resampling.id=resampling.obj$id,
          iteration)
      }
    }
  }
  ml_job_dt <- rbindlist(ml_job_dt_list)
  if(is.function(order_jobs)){
    ord_vec <- order_jobs(ml_job_dt)
    ml_job_dt <- ml_job_dt[ord_vec]
  }
  grid_jobs.rds <- file.path(proj_dir, "grid_jobs.rds")
  saveRDS(ml_job_dt, grid_jobs.rds)
  grid_jobs.csv <- file.path(proj_dir, "grid_jobs.csv")
  out_dt <- ml_job_dt[, .(
    task.i, learner.i, resampling.i, iteration,
    status="not started")]
  fwrite(out_dt, grid_jobs.csv)
  ml_job_dt
}

proj_compute <- function(proj_dir, verbose=FALSE){
  grid_jobs.csv <- file.path(proj_dir, "grid_jobs.csv")
  grid_jobs.csv.lock <- paste0(grid_jobs.csv, ".lock")
  before.lock <- filelock::lock(grid_jobs.csv.lock)
  grid_jobs_dt <- fread(grid_jobs.csv)
  not.started <- grid_jobs_dt$status == "not started"
  grid_job_i <- NULL
  if(any(not.started)){
    grid_job_i <- which(not.started)[1]
    grid_jobs_dt[grid_job_i, status := "started"]
    fwrite(grid_jobs_dt, grid_jobs.csv)
  }
  filelock::unlock(before.lock)
  if(!is.null(grid_job_i)){
    if(verbose)cat(sprintf(
      "Starting ML job %4d / %4d\n", grid_job_i, nrow(grid_jobs_dt)))
    start.time <- Sys.time()
    grid_job_row <- grid_jobs_dt[grid_job_i]
    grid.rds <- file.path(proj_dir, "grid.rds")
    proj.grid <- readRDS(grid.rds)
    this.task <- proj.grid$tasks[[grid_job_row$task.i]]
    this.learner <- proj.grid$learners[[grid_job_row$learner.i]]
    this.resampling <- proj.grid$resamplings[[grid_job_row$resampling.i]]
    this.resampling$instantiate(this.task)
    set_rows <- function(train_or_test){
      train_or_test_set <- paste0(train_or_test, "_set")
      set_fun <- this.resampling[[train_or_test_set]]
      set_fun(grid_job_row$iteration)
    }
    this.learner$train(this.task, set_rows("train"))
    pred <- this.learner$predict(this.task, set_rows("test"))
    result.row <- data.table(
      grid_job_row[, .(task.i, learner.i, resampling.i, iteration)],
      start.time, end.time=Sys.time(),
      process=Sys.getenv("SLURM_ARRAY_TASK_ID", Sys.getpid()),
      learner=list(proj.grid$save_learner(this.learner)),
      pred=list(proj.grid$save_pred(pred)))
    if(is.list(proj.grid$score_args)){
      score_res <- pred$score(proj.grid$score_args)
      set(result.row, j=names(score_res), value=as.list(score_res))
    }
    result.rds <- file.path(proj_dir, "grid_jobs", paste0(grid_job_i, ".rds"))
    dir.create(dirname(result.rds), showWarnings = FALSE)
    saveRDS(result.row, result.rds)
    ## update status.
    after.lock <- filelock::lock(grid_jobs.csv.lock)
    grid_jobs_dt <- fread(grid_jobs.csv)
    grid_jobs_dt[grid_job_i, status := "done"]
    fwrite(grid_jobs_dt, grid_jobs.csv)
    filelock::unlock(after.lock)
    ## check if all done.
    if(verbose)print(grid_jobs_dt[, table(status)])
    if(all(grid_jobs_dt$status=="done")){
      proj_results_save(proj_dir)
    }
    result.row
  }
}

proj_compute_until_done <- function(proj_dir, verbose=FALSE){
  done <- FALSE
  while(!done){
    result <- proj_compute(proj_dir)
    if(is.null(result)){
      done <- TRUE
    }else{
      if(verbose)print(result)
    }
  }
}

proj_results <- function(proj_dir){
  rds.vec <- Sys.glob(file.path(proj_dir, "grid_jobs", "*.rds"))
  res_dt <- rbindlist(lapply(rds.vec, readRDS))
  grid_jobs.rds <- file.path(proj_dir, "grid_jobs.rds")
  ml_job_dt <- readRDS(grid_jobs.rds)
  res_dt[
    ml_job_dt,
    on=c("task.i", "learner.i", "resampling.i", "iteration"),
    nomatch=0L]
}

proj_submit <- function(proj_dir, tasks=2, hours=1, gigabytes=1, verbose=FALSE){
  reg.dir <- file.path(proj_dir, "registry")
  reg <- batchtools::makeRegistry(reg.dir)
  bm.jobs <- batchtools::batchMap(function(i){
    mlr3resampling::proj_compute_until_done(proj_dir, verbose=verbose)
  }, seq_len(tasks))
  if(identical(reg$cluster.functions$name, "Slurm")){
    bm.jobs$chunk <- 1
    resources <- list(
      walltime = 60*60*hours,#seconds
      memory = 1024*gigabytes,#megabytes per cpu
      ncpus=1,  #>1 for multicore/parallel jobs.
      ntasks=1, #>1 for MPI jobs.
      chunks.as.arrayjobs=TRUE)
  }else{
    resources <- list()
  }
  batchtools::submitJobs(bm.jobs, resources)
}

proj_results_save <- function(proj_dir){
  join_dt <- proj_results(proj_dir)
  saveRDS(join_dt, file.path(proj_dir, "results.rds"))
  keep_vec <- sapply(join_dt, is.atomic)
  atomic_dt <- join_dt[, keep_vec, with=FALSE]
  fwrite(atomic_dt, file.path(proj_dir, "results.csv"))
}
