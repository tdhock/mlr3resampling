\name{proj_test}
\alias{proj_test}
\title{
  Test a project with smaller data and fewer resampling iterations
}
\description{
  Runs \code{\link{proj_grid}} to create a new project in the
  \code{test} sub-directory, with a smaller
  number of samples in each task, and with only one iteration
  per Resampling. Runs \code{\link{proj_compute_until_done}} on this new
  test project, and then reads any CSV result files.
}
\usage{
proj_test(proj_dir, min_samples_per_stratum = 10)
}
\arguments{
  \item{proj_dir}{Project directory created by \code{\link{proj_grid}}.}
  \item{min_samples_per_stratum}{
Minimum number of samples to include in the smallest stratum. Other
strata will be down-sampled proportionally.
}
}
\value{
List with names corresponding to CSV files in the test directory, and
values are the data tables that result from \code{fread}.
}
\author{
Toby Dylan Hocking
}
\examples{

  N <- 8000
  set.seed(1)
  reg.dt <- data.table(
    x=runif(N, -2, 2),
    person=factor(rep(c("Alice","Bob"), c(0.1,0.9)*N)))
  reg.pattern.list <- list(
    easy=function(x, person)x^2,
    impossible=function(x, person)(x^2)*(-1)^as.integer(person))
  kfold <- mlr3::ResamplingCV$new()
  kfold$param_set$values$folds <- 2
  reg.task.list <- list()
  for(pattern in names(reg.pattern.list)){
    f <- reg.pattern.list[[pattern]]
    task.dt <- data.table(reg.dt)[
    , y := f(x,person)+rnorm(N, sd=0.5)
    ][]
    task.obj <- mlr3::TaskRegr$new(
      pattern, task.dt, target="y")
    task.obj$col_roles$feature <- "x"
    task.obj$col_roles$stratum <- "person"
    task.obj$col_roles$subset <- "person"
    reg.task.list[[pattern]] <- task.obj
  }
  reg.learner.list <- list(
    featureless=mlr3::LearnerRegrFeatureless$new())
  if(requireNamespace("rpart")){
    reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
  }
  pkg.proj.dir <- tempfile()
  mlr3resampling::proj_grid(
    pkg.proj.dir,
    reg.task.list,
    reg.learner.list,
    kfold,
    save_learner=function(L){
      if(inherits(L, "LearnerRegrRpart")){
        list(rpart=L$model$frame)
      }
    },
    score_args=mlr3::msrs(c("regr.rmse", "regr.mae")))
  mlr3resampling::proj_test(pkg.proj.dir)

}
