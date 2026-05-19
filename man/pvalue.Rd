\name{pvalue}
\alias{pvalue}
\title{
  P-values for comparing Same/Other/All training
}
\description{
  Same/Other/All K-fold cross-validation (SOAK) results in K measures of
  test error/accuracy. This function computes P-values (two-sided
  T-test) between Same and All/Other.
}
\usage{
pvalue(score_in, value.var = NULL, digits=3)
}
\arguments{
  \item{score_in}{
Data table output from \code{\link{score}}.    
}
  \item{value.var}{
    Name of column to use as the evaluation metric in T-test. Default
    NULL means to use the first column matching \code{"classif|regr"}.
}
  \item{digits}{
    Number of decimal places to show for mean and standard deviation.
}
}
\value{
  List of class "pvalue" with named elements \code{value.var},
  \code{stats}, \code{pvalues}.
}
\author{
Toby Dylan Hocking
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
learner_list <- list(
  mlr3::lrn("regr.featureless"),
  if(requireNamespace("rpart"))mlr3::lrn("regr.rpart"))
bgrid <- mlr3::benchmark_grid(sim.task, learner_list, soak)
result <- mlr3::benchmark(bgrid)
score.dt <- mlr3resampling::score(result, mlr3::msr("regr.rmse"))
if(requireNamespace("ggplot2")) plot(score.dt)

p.list <- mlr3resampling::pvalue(score.dt)
if(requireNamespace("ggplot2")) plot(p.list)

}
