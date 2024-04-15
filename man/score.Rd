\name{score}
\alias{score}
\title{
  Score benchmark results
}
\description{
  Computes a data table of scores.
}
\usage{
score(bench.result, ...)
}
\arguments{
  \item{bench.result}{
    Output of \code{\link[mlr3:benchmark]{benchmark()}}.
  }
  \item{\dots}{
    Additional arguments to pass to \code{bench.result$score}, for
    example \code{measures}. 
  }
}
\value{
  data table with scores.
}
\author{
  Toby Dylan Hocking
}
\examples{

N <- 100
library(data.table)
set.seed(1)
reg.dt <- data.table(
  x=runif(N, -2, 2),
  person=rep(1:2, each=0.5*N))
reg.pattern.list <- list(
  easy=function(x, person)x^2,
  impossible=function(x, person)(x^2+person*3)*(-1)^person)
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
same_other <- mlr3resampling::ResamplingSameOtherSizesCV$new()
reg.learner.list <- list(
  if(requireNamespace("rpart"))mlr3::LearnerRegrRpart$new(),
  mlr3::LearnerRegrFeatureless$new())
(bench.grid <- mlr3::benchmark_grid(
  reg.task.list,
  reg.learner.list,
  same_other))
bench.result <- mlr3::benchmark(bench.grid)
bench.score <- mlr3resampling::score(bench.result)
if(require(animint2)){
  ggplot()+
    geom_point(aes(
      regr.mse, train.subsets, color=algorithm),
      shape=1,
      data=bench.score)+
    facet_grid(
      test.subset ~ task_id,
      labeller=label_both,
      scales="free")+
    scale_x_log10()
}

}
