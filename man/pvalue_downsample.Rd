\name{pvalue_downsample}
\alias{pvalue_downsample}
\title{
  P-values for full versus down-sampled SOAK results
}
\description{
  For one \code{test.subset}, compute summary statistics and paired
  two-sided t-test P-values that compare \code{same} versus
  \code{all}/\code{other}, for two train-size panels:
  full (\code{n.train.groups == groups}) and down-sampled
  (\code{n.train.groups == min(groups)}).
}
\usage{
pvalue_downsample(
  score_in,
  subset_name,
  model_name,
  value.var = NULL,
  digits = 3
)
}
\arguments{
  \item{score_in}{
Data table output from \code{\link{score}}, produced using
\code{ResamplingSameOtherSizesCV} with down-sampling
(typically \code{sizes >= 0}).
}
  \item{subset_name}{
Length-1 character string; exact value to match in
\code{score_in$test.subset}.
}
  \item{model_name}{
Length-1 character string; exact value to match in
\code{score_in$algorithm}.
}
  \item{value.var}{
    Name of column to use as the evaluation metric in T-test. Default
    NULL means to use the first column matching \code{"classif|regr"}.
}
  \item{digits}{
Non-negative integer number of digits to print in mean/SD text labels.
}
}
\value{
  List of class \code{"pvalue_downsample"} with named elements
  \code{subset_name}, \code{model_name}, \code{value.var},
  \code{stats}, and \code{pvalues}.
}
\author{
Daniel Agyapong
}
\examples{
set.seed(1)
N <- 40L
task.dt <- data.table::data.table(
  x = runif(N, -2, 2),
  subset_col = factor(rep(c("F", "M"), each = N/2))
)[, y := x^2 + rnorm(.N, sd = 0.2)]
task <- mlr3::TaskRegr$new("toy_soak", task.dt, target = "y")
task$col_roles$feature <- "x"
task$col_roles$subset <- "subset_col"
task$col_roles$stratum <- "subset_col"
soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
soak$param_set$values$folds <- 2L
soak$param_set$values$seeds <- 1L
soak$param_set$values$sizes <- 0L
grid <- mlr3::benchmark_grid(
  task,
  list(mlr3::LearnerRegrFeatureless$new()),
  soak
)
result <- mlr3::benchmark(grid)
score.dt <- mlr3resampling::score(result, mlr3::msr("regr.rmse"))
model_name <- unique(score.dt$algorithm)[1]
down.list <- mlr3resampling::pvalue_downsample(score.dt, "F", model_name)
if(requireNamespace("ggplot2", quietly = TRUE))plot(down.list)
}
