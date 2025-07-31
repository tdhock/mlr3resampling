\name{Learners}
\alias{AutoTunerTorch_epochs}
\alias{LearnerRegrCVGlmnetSave}
\alias{LearnerClassifCVGlmnetSave}
\title{
  Learner classes with special methods
}
\description{
  \code{AutoTunerTorch_epochs} inherits from
  \code{mlr3tuning::AutoTuner}, with an \code{initialize} method that
  takes arguments to construct a torch module learner. It runs gradient
  descent up to \code{max_epochs} and then re-runs using the best number
  of epochs. Its \code{edit_learner} method sets number of epochs to 2
  (for quick learning during \code{\link{proj_test}}),
  and its \code{save_learner} method returns a history data table (one
  row per epoch).
  \code{LearnerRegrCVGlmnetSave} inherits from
  \code{LearnerRegrCVGlmnet}; its \code{save_learner} method returns a
  data table of regularized linear model weights (no \code{edit_learner}
  method).
  \code{LearnerClassifCVGlmnetSave} is similar.
}
\author{
  Toby Dylan Hocking
}
\examples{

## Simulate regression data.
N <- 80
library(data.table)
set.seed(1)
reg.dt <- data.table(
  x=runif(N, -2, 2),
  noise=runif(N, -2, 2),
  person=factor(rep(c("Alice","Bob"), each=0.5*N)))
reg.pattern.list <- list(
  easy=function(x, person)x^3,
  impossible=function(x, person)(x^2)*(-1)^as.integer(person))
SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
reg.task.list <- list()
for(pattern in names(reg.pattern.list)){
  f <- reg.pattern.list[[pattern]]
  task.dt <- data.table(reg.dt)[
  , y := f(x,person)+rnorm(N, sd=0.5)
  ][]
  task.obj <- mlr3::TaskRegr$new(
    pattern, task.dt, target="y")
  task.obj$col_roles$feature <- c("x","noise")
  task.obj$col_roles$stratum <- "person"
  task.obj$col_roles$subset <- "person"
  reg.task.list[[pattern]] <- task.obj
}
reg.task.list # two regression tasks.

## Create a list of learners.
reg.learner.list <- list(
  featureless=mlr3::LearnerRegrFeatureless$new())
if(requireNamespace("mlr3torch") && torch::torch_is_installed()){
  gen_linear <- torch::nn_module(
    "my_linear",
    initialize = function(task) {
      self$weight = torch::nn_linear(task$n_features, 1)
    },
    forward = function(x) {
      self$weight(x)
    }
  )
  reg.learner.list$torch_linear <- mlr3resampling::AutoTunerTorch_epochs$new(
    "torch_linear",
    module_generator=gen_linear,
    max_epochs=3,
    batch_size=10,
    loss=mlr3torch::t_loss("mse"),
    measure_list=mlr3::msrs(c("regr.mse","regr.mae")))
}
if(requireNamespace("mlr3learners")){
  reg.learner.list$cv_glmnet <- mlr3resampling::LearnerRegrCVGlmnetSave$new()
  reg.learner.list$cv_glmnet$param_set$values$nfolds <- 3
}
reg.learner.list # a list of learners.

# 2-fold CV.
kfold <- mlr3::ResamplingCV$new()
kfold$param_set$values$folds <- 2

# Create project grid.
pkg.proj.dir <- tempfile()
pgrid <- mlr3resampling::proj_grid(
  pkg.proj.dir,
  reg.task.list,    
  reg.learner.list,
  score_args=mlr3::msrs("regr.rmse"),
  kfold)

\dontrun{
test_out <- mlr3resampling::proj_test(pkg.proj.dir)
test_out$learners_history.csv # from AutoTunerTorch_epochs, 2 epochs for testing.
test_out$learners_weights.csv # from LearnerRegrCVGlmnetSave
torch.job.i <- which(pgrid$learner_id=="torch_linear")[1]
mlr3resampling::proj_compute(torch.job.i, pkg.proj.dir)
}

}
