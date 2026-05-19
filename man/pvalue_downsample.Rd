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

data.table::setDTthreads(1L)
library(data.table)
set.seed(2)
N <- 150L
x <- runif(N, -3, 3)
sim.dt <- data.table(
  x = x,
  y = sin(x) + rnorm(N, sd = 0.5),
  Subset = rep(c("large", "large", "small"), length.out = N))
sim.task <- mlr3::TaskRegr$new("iid_easy", sim.dt, target = "y")
sim.task$col_roles$feature <- "x"
sim.task$col_roles$subset <- "Subset"
soak <- mlr3resampling::ResamplingSameOtherSizesCV$new()
soak$param_set$values$folds <- 5L
soak$param_set$values$sizes <- 0L
learner_list <- list(
  mlr3::lrn("regr.featureless"),
  if(requireNamespace("rpart"))mlr3::lrn("regr.rpart"))
bgrid <- mlr3::benchmark_grid(sim.task, learner_list, soak)
result <- mlr3::benchmark(bgrid)
score.dt <- mlr3resampling::score(result, mlr3::msr("regr.rmse"))
if(requireNamespace("ggplot2")) plot(score.dt)

down.list <- mlr3resampling::pvalue_downsample(score.dt[
  task_id == "iid_easy" & test.subset == "large" & algorithm == "rpart"])
if(requireNamespace("ggplot2")) plot(down.list)

}
