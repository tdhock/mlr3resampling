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
pvalue_downsample(score_in, subset_name, model_name)
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
\dontrun{
bench.score <- mlr3resampling::score(bench.result, mlr3::msr("regr.rmse"))
down.list <- mlr3resampling::pvalue_downsample(bench.score, "F", "rpart")
plot(down.list)
}
}
