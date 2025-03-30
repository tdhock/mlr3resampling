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
pvalue(score_in, value.var = NULL)
}
\arguments{
  \item{score_in}{
Data table output from \code{\link{score}}.    
}
  \item{value.var}{
    Name of column to use as the evaluation metric in T-test. Default
    NULL means to use the first column matching \code{"classif|regr"}.
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

N <- 80
library(data.table)
set.seed(1)
reg.dt <- data.table(
  x=runif(N, -2, 2),
  person=rep(1:2, each=0.5*N))
reg.pattern.list <- list(
  easy=function(x, person)x^2,
  impossible=function(x, person)(x^2)*(-1)^person)
SOAK <- mlr3resampling::ResamplingSameOtherSizesCV$new()
reg.task.list <- list()
for(pattern in names(reg.pattern.list)){
  f <- reg.pattern.list[[pattern]]
  yname <- paste0("y_",pattern)
  reg.dt[, (yname) := f(x,person)+rnorm(N, sd=0.5)][]
  task.dt <- reg.dt[, c("x","person",yname), with=FALSE]
  task.obj <- mlr3::TaskRegr$new(
    pattern, task.dt, target=yname)
  task.obj$col_roles$stratum <- "person"
  task.obj$col_roles$subset <- "person"
  reg.task.list[[pattern]] <- task.obj
}
reg.learner.list <- list(
  mlr3::LearnerRegrFeatureless$new())
if(requireNamespace("rpart")){
  reg.learner.list$rpart <- mlr3::LearnerRegrRpart$new()
}
(bench.grid <- mlr3::benchmark_grid(
  reg.task.list,
  reg.learner.list,
  SOAK))
bench.result <- mlr3::benchmark(bench.grid)
bench.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
bench.plist <- mlr3resampling::pvalue(bench.score)
plot(bench.plist)

}
