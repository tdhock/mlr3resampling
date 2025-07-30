proj_jobs_read <- function(proj_dir){
  fread(file.path(proj_dir, "grid_jobs.csv"))
}

proj_todo <- function(proj_dir){
  ml_job_dt <- proj_jobs_read(proj_dir)
  expected.rds <- file.path(
    proj_dir, "grid_jobs", paste0(1:nrow(ml_job_dt), ".rds"))
  which(!file.exists(expected.rds))
}

edit_learner_default <- function(L){
  if(is.function(L$edit_learner))return(L$edit_learner())
  if(inherits(L, "AutoTuner")){
    learner <- L$learner
    if(inherits(learner, "GraphLearner")){
      learner <- learner$base_learner()
    }
    if(inherits(learner, "LearnerTorch")){
      learner$param_set$set_values(
        patience=2,
        epochs=paradox::to_tune(upper=2, internal=TRUE)
      )
    }
  }
}

save_learner_default <- function(L){
  if(is.function(L$save_learner))return(L$save_learner())
}

proj_test <- function(proj_dir, min_samples_per_stratum = 10, edit_learner=edit_learner_default, max_jobs=Inf){
  . <- ..batch.i <- ..row.id <- ..strat.i <- max.i <- NULL
  ## Above to avoid CRAN NOTE.
  proj.grid <- readRDS(file.path(proj_dir, "grid.rds"))
  proj.grid$order_jobs <- function(DT){
    indices <- which(DT$iteration==1)
    indices[seq(1, min(length(indices), max_jobs))]
  }
  ml_job_dt <- proj_jobs_read(proj_dir)
  ml_ord_dt <- ml_job_dt[proj.grid$order_jobs(ml_job_dt)]
  for(task.i in unique(ml_ord_dt$task.i)){
    task.rds <- file.path(proj_dir, "tasks", paste0(task.i, ".rds"))
    this.task <- readRDS(task.rds)
    stratum <- this.task$col_roles$stratum
    strat_dt <- if(length(stratum)){
      this.task$data(cols=stratum)
    }else{
      data.table(stratum=rep(1L, this.task$nrow))
    }
    strat_dt[
    , ..strat.i := 1:.N, by=stratum
    ][
    , ..row.id := this.task$row_ids
    ][]
    count_dt <- strat_dt[, .(max.i=max(..strat.i)), by=stratum][order(max.i)]
    count_min <- count_dt$max.i[1]
    some.ids <- strat_dt[
    , ..batch.i := ..strat.i/max(..strat.i)*count_min, by=stratum
    ][
      ..batch.i <= min_samples_per_stratum
    ]$..row.id
    this.task$filter(some.ids)
    proj.grid$tasks[[paste(task.i)]] <- this.task
  }
  lapply(proj.grid$learners, edit_learner)
  proj.grid$proj_dir <- file.path(proj_dir, "test")
  unlink(proj.grid$proj_dir, recursive = TRUE)
  grid_dt <- do.call(proj_grid, proj.grid)
  proj_compute_all(proj.grid$proj_dir)
  proj_fread(proj.grid$proj_dir)
}

proj_fread <- function(proj_dir){
  csv_list <- Sys.glob(file.path(proj_dir, "*.csv"))
  out_list <- list()
  for(csv_i in seq_along(csv_list)){
    out_csv <- csv_list[[csv_i]]
    out_list[[basename(out_csv)]] <- fread(out_csv)
  }
  out_list
}  

proj_grid <- function(proj_dir, tasks, learners, resamplings, order_jobs=NULL, score_args=NULL, save_learner=save_learner_default, save_pred=FALSE){
  . <- n.train.groups <- NULL
  ## Above to avoid CRAN NOTE.
  if(file.exists(proj_dir)){
    stop(proj_dir, " already exists, so not over-writing")
  }
  dir.create(proj_dir, showWarnings = FALSE)
  on.exit(unlink(proj_dir, recursive=TRUE))
  if(is.null(score_args) && isFALSE(save_pred)){
    warning("no score_args nor save_pred, so there will no test error results")
  }
  proj.grid <- list()
  for(arg in c("tasks", "learners", "resamplings")){
    value <- get(arg)
    if(!is.list(value)){
      value <- list(value)
    }
    if(length(value)==0)stop(arg, " is empty, but need at least one")
    proj.grid[[arg]] <- value
  }
  do.call(mlr3::benchmark_grid, proj.grid) # error checking.
  proj.grid$score_args <- score_args
  for(fun_name in paste0("save_",c("learner","pred"))){
    fun <- get(fun_name)
    if(identical(fun, FALSE)){
      fun <- function(L)NULL
    }else if(identical(fun, TRUE)){
      fun <- function(L)L
    }else if(!is.function(fun)){
      stop(fun_name, " should be a function")
    }
    proj.grid[[fun_name]] <- fun
  }
  ml_job_dt_list <- list()
  for(task.i in seq_along(proj.grid$tasks)){
    task.obj <- proj.grid$tasks[[task.i]]
    tasks.dir <- file.path(proj_dir, "tasks")
    dir.create(tasks.dir, showWarnings = FALSE)
    task.rds <- file.path(tasks.dir, paste0(task.i, ".rds"))
    saveRDS(task.obj, task.rds)
    for(resampling.i in seq_along(proj.grid$resamplings)){
      resampling.obj <- proj.grid$resamplings[[resampling.i]]$clone()
      resampling.obj$instantiate(task.obj)
      iteration <- resampling.obj$instance$iteration.dt
      if(is.null(iteration)){
        iteration <- seq_len(resampling.obj$iters)
      }
      it_dt <- data.table(iteration)
      for(it in 1:nrow(it_dt)){
        resampling_list <- list()
        for(train_or_test in c("train","test")){
          train_or_test_set <- paste0(train_or_test, "_set")
          set_fun <- resampling.obj[[train_or_test_set]]
          resampling_list[[train_or_test]] <- set_fun(it)
        }
        resampling.rds <- file.path(
          proj_dir, "resamplings", task.i, resampling.i, paste0(it, ".rds"))
        parent.dir <- dirname(resampling.rds)
        dir.create(parent.dir, recursive = TRUE, showWarnings = FALSE)
        saveRDS(resampling_list, resampling.rds)
      }
      for(learner.i in seq_along(proj.grid$learners)){
        ml_job_dt_list[[paste(
          task.i, resampling.i, learner.i
        )]] <- data.table(
          task.i, learner.i, resampling.i,
          task_id=task.obj$id,
          learner_id=proj.grid$learners[[learner.i]]$id,
          resampling_id=resampling.obj$id,
          iteration)
      }
    }
  }
  ml_job_dt <- rbindlist(ml_job_dt_list)
  if(is.null(order_jobs))order_jobs <- function(DT){
    if("n.train.groups" %in% names(DT))DT[, order(-n.train.groups)]
    else 1:nrow(DT)
  }
  if(is.function(order_jobs)){
    ord_vec <- order_jobs(ml_job_dt)
    if(!is.integer(ord_vec))stop("order_jobs should return integer")
    ml_job_dt <- ml_job_dt[ord_vec]
  }
  task.i.max <- max(ml_job_dt$task.i)
  proj.grid$tasks <- proj.grid$tasks[seq(1, task.i.max)]
  proj.grid$tasks <- NULL
  saveRDS(proj.grid, file.path(proj_dir, "grid.rds"))
  keep <- sapply(ml_job_dt, is.atomic)
  out_dt <- ml_job_dt[, keep, with=FALSE]
  fwrite(out_dt, file.path(proj_dir, "grid_jobs.csv"))
  if(basename(proj_dir)!="test")message(sprintf('grid with %d jobs created! Test one job with the following code in a new R session:\nmlr3resampling::proj_test("%s", max_jobs=1)', nrow(out_dt), normalizePath(proj_dir)))
  on.exit()
  out_dt
}

proj_compute <- function(grid_job_i, proj_dir, verbose=FALSE){
  status <- . <- task.i <- learner.i <- resampling.i <- iteration <- NULL
  ## Above to avoid CRAN NOTE.
  grid_jobs_dt <- proj_jobs_read(proj_dir)
  if(is.numeric(grid_job_i))grid_job_i <- as.integer(grid_job_i)
  if(!(
    is.integer(grid_job_i) && length(grid_job_i)==1 && is.finite(grid_job_i) &&
      1 <= grid_job_i && grid_job_i <= nrow(grid_jobs_dt)
  )){
    stop("grid_job_i must be integer from 1 to ", nrow(grid_jobs_dt))
  }
  if(verbose)cat(sprintf(
    "Starting ML job %4d / %4d\n", grid_job_i, nrow(grid_jobs_dt)))
  start.time <- Sys.time()
  grid_job_row <- grid_jobs_dt[grid_job_i]
  grid.rds <- file.path(proj_dir, "grid.rds")
  proj.grid <- readRDS(grid.rds)
  task.rds <- file.path(proj_dir, "tasks", paste0(grid_job_row$task.i, ".rds"))
  this.task <- readRDS(task.rds)
  this.learner <- proj.grid$learners[[grid_job_row$learner.i]]
  resampling_list <- readRDS(grid_job_row[, file.path(
    proj_dir, "resamplings", task.i, resampling.i, paste0(iteration, ".rds"))])
  this.learner$train(this.task, resampling_list$train)
  pred <- this.learner$predict(this.task, resampling_list$test)
  result.row <- data.table(
    grid_job_row[, .(task.i, learner.i, resampling.i, iteration)],
    start.time, end.time=Sys.time(),
    process=tryCatch(pbdMPI::comm.rank(), error=function(e)NA_integer_),
    learner=list(proj.grid$save_learner(this.learner)),
    pred=list(proj.grid$save_pred(pred)))
  if(is.list(proj.grid$score_args)){
    score_res <- pred$score(proj.grid$score_args)
    set(result.row, j=names(score_res), value=as.list(score_res))
  }
  result.rds <- file.path(proj_dir, "grid_jobs", paste0(grid_job_i, ".rds"))
  dir.create(dirname(result.rds), showWarnings = FALSE)
  saveRDS(result.row, result.rds)
  result.row
}

proj_results <- function(proj_dir, verbose=FALSE){
  rds.vec <- Sys.glob(file.path(proj_dir, "grid_jobs", "*.rds"))
  res_dt_list <- list()
  for(job.i in seq_along(rds.vec)){
    job.rds <- rds.vec[[job.i]]
    state <- tryCatch({
      res_dt_list[[job.rds]] <- readRDS(job.rds)
      "OK"
    }, error=function(e){
      sprintf("%s: %s", class(e)[1], e[["message"]])
    })
    if(verbose)cat(sprintf("%4d / %4d %s %s\n", job.i, length(rds.vec), job.rds, state))
  }
  res_dt <- rbindlist(res_dt_list)
  ml_job_dt <- proj_jobs_read(proj_dir)
  res_dt[
    ml_job_dt,
    on=c("task.i", "learner.i", "resampling.i", "iteration"),
    nomatch=0L]
}

proj_submit <- function(proj_dir, tasks=2, hours=1, gigabytes=1, verbose=FALSE){
  proj_dir <- normalizePath(proj_dir, mustWork=TRUE)
  param <- function(name, ...){
    paste0("#SBATCH --", name, "=", ...)
  }
  MPI.out <- file.path(proj_dir, "MPI.out")
  MPI.R <- file.path(proj_dir, "MPI.R")
  sh_code <- paste(c(
    "#!/bin/bash",
    param("ntasks", tasks),
    param("time", hours*60),
    param("mem-per-cpu", gigabytes, "G"),
    param("cpus-per-task", 1),
    param("output", MPI.out),
    param("error", MPI.out),
    paste("srun Rscript", MPI.R)
  ), collapse="\n")
  MPI.sh <- file.path(proj_dir, "MPI.sh")
  cat(sh_code, file=MPI.sh)
  R_code <- sprintf('mlr3resampling::proj_compute_mpi("%s")', proj_dir)
  cat(R_code, file=MPI.R)
  out <- system(paste("sbatch", MPI.sh), intern=TRUE)
  gsub("[^0-9]", "", out)
}

proj_compute_mpi <- function(proj_dir, verbose=FALSE){
  todo.i.vec <- proj_todo(proj_dir)
  dt_list <- pbdMPI::task.pull(todo.i.vec, proj_compute, proj_dir)
  if(pbdMPI::comm.rank()==0) proj_results_save(proj_dir)
  pbdMPI::finalize()
  rbindlist(dt_list)
}

proj_compute_all <- function(proj_dir, verbose=FALSE){
  todo.i.vec <- proj_todo(proj_dir)
  dt_list <- lapply(todo.i.vec, proj_compute, proj_dir)
  proj_results_save(proj_dir)
  rbindlist(dt_list)
}

proj_results_save <- function(proj_dir, verbose=FALSE){
  learner <- NULL
  ## above for CRAN check.
  only_atomic <- function(in_dt){
    keep_vec <- sapply(in_dt, is.atomic)
    in_dt[, keep_vec, with=FALSE]
  }
  fwrite_atomic <- function(in_dt, pre){
    fwrite(only_atomic(in_dt), file.path(proj_dir, paste0(pre, ".csv")))
  }
  save_df <- function(suffix, out.df){
    pre <- paste0("learners", suffix)
    learner_out_list[[pre]][[paste(row.i)]] <<- data.table(atomic_row, out.df)
  }
  join_dt <- proj_results(proj_dir, verbose=verbose)
  saveRDS(join_dt, file.path(proj_dir, "results.rds"))
  learner_out_list <- list()
  for(row.i in 1:nrow(join_dt)){
    join_row <- join_dt[row.i]
    atomic_row <- only_atomic(join_row)
    L <- join_row$learner[[1]]
    suffix <- NULL
    if(is.data.frame(L)){
      save_df("", L)
    }
    if(is.list(L) && is.character(names(L))){
      for(out.i in seq_along(L)){
        out.name <- names(L)[[out.i]]
        out.df <- L[[out.i]]
        if(out.name != "" && is.data.frame(out.df)){
          save_df(paste0("_",out.name), out.df)
        }
      }
    }
  }
  for(pre in names(learner_out_list)){
    learner_dt <- rbindlist(learner_out_list[[pre]])
    if(nrow(learner_dt))fwrite_atomic(learner_dt, pre)
  }
  fwrite_atomic(join_dt, "results")
}
