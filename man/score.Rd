\name{score}
\alias{score}
\title{
  Score benchmark results
}
\description{
  Computes a data table of scores, joined with original group columns.
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
  data table with scores and original values from columns specified with
  group role.
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
  reg.task.list[[pattern]] <- mlr3::TaskRegr$new(
    pattern, task.dt, target=yname
  )$set_col_roles("person",c("group","stratum"))
}
same_other <- mlr3resampling::ResamplingSameOtherCV$new()
same_other$param_set$values$folds <- 3
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
      regr.mse, train.groups, color=algorithm),
      shape=1,
      data=bench.score)+
    facet_grid(
      person ~ task_id,
      labeller=label_both,
      scales="free")+
    scale_x_log10()
}

if(require(animint2)){
  inst <- bench.score$resampling[[1]]$instance
  rect.expand <- 0.2
  ggplot()+
    ggtitle("All train/test splits to compute")+
    theme_bw()+
    facet_grid(
      . ~ train.groups,
      labeller=label_both,
      scales="free",
      space="free")+
    scale_size_manual(values=c(group=3, fold=1))+
    scale_color_manual(values=c(group="orange", fold="grey50"))+
    geom_rect(aes(
      xmin=-Inf, xmax=Inf,
      color=rows,
      size=rows,
      ymin=display_row, ymax=display_end),
      fill=NA,
      data=inst$viz.rect.dt)+
    geom_rect(aes(
      xmin=iteration-rect.expand, ymin=display_row,
      xmax=iteration+rect.expand, ymax=display_end,
      fill=set.name),
      data=inst$viz.set.dt)+
    geom_text(aes(
      ifelse(rows=="group", Inf, -Inf),
      (display_row+display_end)/2,
      hjust=ifelse(rows=="group", 1, 0),
      label=paste0(rows, "=", ifelse(rows=="group", group, fold))),
      data=data.table(train.name="same", inst$viz.rect.dt))+
    scale_x_continuous(
      "Split number")+
    scale_y_continuous(
      "Row number (data sorted by group, fold)")
}

}
