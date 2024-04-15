\name{AZtrees}
\alias{AZtrees}
\docType{data}
\title{
 Arizona Trees
}
\description{
  Classification data set with polygons (groups which should not be
  split in CV) and subsets (region3 or region4).
}
\usage{data("AZtrees")}
\format{
  A data frame with 5956 observations on the following 25 variables.
  \describe{
    \item{\code{region3}}{a character vector}
    \item{\code{region4}}{a character vector}
    \item{\code{polygon}}{a numeric vector}
    \item{\code{y}}{a character vector}
    \item{\code{SAMPLE_1}}{a numeric vector}
    \item{\code{SAMPLE_2}}{a numeric vector}
    \item{\code{SAMPLE_3}}{a numeric vector}
    \item{\code{SAMPLE_4}}{a numeric vector}
    \item{\code{SAMPLE_5}}{a numeric vector}
    \item{\code{SAMPLE_6}}{a numeric vector}
    \item{\code{SAMPLE_7}}{a numeric vector}
    \item{\code{SAMPLE_8}}{a numeric vector}
    \item{\code{SAMPLE_9}}{a numeric vector}
    \item{\code{SAMPLE_10}}{a numeric vector}
    \item{\code{SAMPLE_11}}{a numeric vector}
    \item{\code{SAMPLE_12}}{a numeric vector}
    \item{\code{SAMPLE_13}}{a numeric vector}
    \item{\code{SAMPLE_14}}{a numeric vector}
    \item{\code{SAMPLE_15}}{a numeric vector}
    \item{\code{SAMPLE_16}}{a numeric vector}
    \item{\code{SAMPLE_17}}{a numeric vector}
    \item{\code{SAMPLE_18}}{a numeric vector}
    \item{\code{SAMPLE_19}}{a numeric vector}
    \item{\code{SAMPLE_20}}{a numeric vector}
    \item{\code{SAMPLE_21}}{a numeric vector}
  }
}
\source{
  Paul Nelson Arellano, paul.arellano@nau.edu
}
\examples{

data(AZtrees)
task.obj <- mlr3::TaskClassif$new("AZtrees3", AZtrees, target="y")
task.obj$col_roles$feature <- grep("SAMPLE", names(AZtrees), value=TRUE)
task.obj$col_roles$group <- "polygon"
task.obj$col_roles$subset <- "region3"
str(task.obj)
same_other_sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
same_other_sizes_cv$instantiate(task.obj)
same_other_sizes_cv$instance$iteration.dt

}
\keyword{datasets}
