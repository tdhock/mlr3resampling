\name{ResamplingSameOther}
\alias{ResamplingSameOther}
\alias{ResamplingSameOtherCV}
\title{Resampling for comparing training on same or other groups}
\description{
\code{\link{ResamplingSameOther}} is the abstract base class for
\code{\link{ResamplingSameOtherCV}},
which defines how a task is partitioned for
resampling, for example in
\code{\link[mlr3:resample]{resample()}} or
\code{\link[mlr3:benchmark]{benchmark()}}.

Resampling objects can be instantiated on a
\code{\link[mlr3:Task]{Task}},
which should define at least one group variable.

After instantiation, sets can be accessed via
\verb{$train_set(i)} and
\verb{$test_set(i)}, respectively. 
}
\details{
  A supervised learning algorithm inputs a train set, and outputs a
  prediction function, which can be used on a test set. If each data
  point belongs to a group (such as geographic region, year, etc), then
  how do we know if it is possible to train on one group, and predict
  accurately on another group? Cross-validation can be used to determine
  the extent to which this is possible, by first assigning fold IDs from
  1 to K to all data (possibly using stratification, usually by group
  and label). Then we loop over test sets (group/fold combinations),
  train sets (same group, other groups, all groups), and compute
  test/prediction accuracy for each combination.  Comparing
  test/prediction accuracy between same and other, we can determine the
  extent to which it is possible (perfect if same/other have similar
  test accuracy for each group; other is usually somewhat less accurate
  than same; other can be just as bad as featureless baseline when the
  groups have different patterns).
}
\section{Stratification}{

\code{\link{ResamplingSameOtherCV}} supports stratified sampling.
The stratification variables are assumed to be discrete,
and must be stored in the \link{Task} with column role \code{"stratum"}.
In case of multiple stratification variables,
each combination of the values of the stratification variables forms a stratum.
}

\section{Grouping}{

\code{\link{ResamplingSameOtherCV}} supports grouping of observations.
The grouping variable is assumed to be discrete,
and must be stored in the \link{Task} with column role \code{"group"}.

Then number of cross-validation folds K should be defined as the
\code{fold} parameter.

In each group, there will be about an equal number of observations
assigned to each of the K folds.
The assignments are stored in
\verb{$instance$id.dt}.
The train/test splits are defined by all possible combinations of
test group, test fold, and train groups (same/other/all).
The splits are stored in
\verb{$instance$iteration.dt}.
}

\examples{
same_other <- mlr3resampling::ResamplingSameOtherCV$new()
same_other$param_set$values$folds <- 5
}
\seealso{
  \itemize{
    \item Blog post
    \url{https://tdhock.github.io/blog/2023/R-gen-new-subsets/}
    \item Package \CRANpkg{mlr3} for standard
    \code{\link[mlr3:Resampling]{Resampling}}, which does not support comparing
    train on same or other groups.
    \item \code{\link{score}} and Simulations vignette for more detailed examples.
  }
}
\concept{Resampling}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Resampling-new}{\code{Resampling$new()}}
\item \href{#method-Resampling-train_set}{\code{Resampling$train_set()}}
\item \href{#method-Resampling-test_set}{\code{Resampling$test_set()}}
}
}

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Resampling-new"></a>}}
\if{latex}{\out{\hypertarget{method-Resampling-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Resampling$new(
  id,
  param_set = ps(),
  duplicated_ids = FALSE,
  label = NA_character_,
  man = NA_character_
)}\if{html}{\out{</div>}}
}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier for the new instance.}

\item{\code{param_set}}{(\link[paradox:ParamSet]{paradox::ParamSet})\cr
Set of hyperparameters.}

\item{\code{duplicated_ids}}{(\code{logical(1)})\cr
Set to \code{TRUE} if this resampling strategy may have duplicated row ids in a single training set or test set.

Note that this object is typically constructed via a derived classes, e.g. \link{ResamplingCV} or \link{ResamplingHoldout}.}

\item{\code{label}}{(\code{character(1)})\cr
Label for the new instance.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.
The referenced help package can be opened via method \verb{$help()}.}
}
\if{html}{\out{</div>}}
}
}

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Resampling-train_set"></a>}}
\if{latex}{\out{\hypertarget{method-Resampling-train_set}{}}}
\subsection{Method \code{train_set()}}{
Returns the row ids of the i-th training set.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Resampling$train_set(i)}\if{html}{\out{</div>}}
}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{i}}{(\code{integer(1)})\cr
Iteration.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{integer()}) of row ids.
}
}

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Resampling-test_set"></a>}}
\if{latex}{\out{\hypertarget{method-Resampling-test_set}{}}}
\subsection{Method \code{test_set()}}{
Returns the row ids of the i-th test set.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Resampling$test_set(i)}\if{html}{\out{</div>}}
}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{i}}{(\code{integer(1)})\cr
Iteration.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\code{integer()}) of row ids.
}
}

}
