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
  value.var = NULL,
  digits = 3
)
}
\arguments{
  \item{score_in}{
Data table output from \code{\link{score}}, produced using
\code{ResamplingSameOtherSizesCV} with down-sampling
(typically \code{sizes >= 0}). If multiple \code{task_id},
\code{test.subset}, or \code{algorithm} values are present, the first
row values are used.
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
  \code{stats}, and \code{pvalues}. \code{subset_name} and
  \code{model_name} are inferred from \code{score_in}.
}
\author{
Daniel Agyapong
}
\examples{
library(data.table)
set.seed(1)
N <- 120L
x <- runif(N, -3, 3)
sim.dt <- data.table(
  x = x,
  y = sin(x) + rnorm(N, sd = 0.3),
  Subset = rep(c("large", "large", "small"), length.out = N)
)
task <- mlr3::TaskRegr$new("iid_easy", sim.dt, target = "y")
task$col_roles$feature <- "x"
task$col_roles$subset <- "Subset"
soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
soak$param_set$values$folds <- 2L
soak$param_set$values$seeds <- 1L
soak$param_set$values$sizes <- 0L
if(requireNamespace("rpart", quietly = TRUE)){
  grid <- mlr3::benchmark_grid(task, list(mlr3::lrn("regr.rpart")), soak)
  result <- mlr3::benchmark(grid)
  score.dt <- mlr3resampling::score(result)[, .(
    task_id, test.subset, test.fold, train.subsets,
    groups, n.train.groups, algorithm, RMSE = sqrt(regr.mse)
  )]
  down.list <- mlr3resampling::pvalue_downsample(
    score.dt[task_id == "iid_easy" & test.subset == "small" & algorithm == "rpart"],
    value.var = "RMSE",
    digits = 3
  )
  if(requireNamespace("ggplot2", quietly = TRUE)) plot(down.list)
}
}
