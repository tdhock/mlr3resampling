\name{ResamplingVariableSizeTrain}
\alias{ResamplingVariableSizeTrain}
\alias{ResamplingVariableSizeTrainCV}
\title{Resampling for comparing training on same or other groups}
\description{
\code{\link{ResamplingVariableSizeTrain}} is the abstract base class for
\code{\link{ResamplingVariableSizeTrainCV}},
which defines how a task is partitioned for
resampling, for example in
\code{\link[mlr3:resample]{resample()}} or
\code{\link[mlr3:benchmark]{benchmark()}}.

Resampling objects can be instantiated on a
\code{\link[mlr3:Task]{Task}}.

After instantiation, sets can be accessed via
\verb{$train_set(i)} and
\verb{$test_set(i)}, respectively. 
}
\details{
  A supervised learning algorithm inputs a train set, and outputs a
  prediction function, which can be used on a test set.
  How many train samples are required to get accurate predictions on a
  test set? Cross-validation can be used to answer this question, with
  variable size train sets.
}
\section{Stratification}{

\code{\link{ResamplingVariableSizeTrainCV}} supports stratified sampling.
The stratification variables are assumed to be discrete,
and must be stored in the \link{Task} with column role \code{"stratum"}.
In case of multiple stratification variables,
each combination of the values of the stratification variables forms a stratum.
}

\section{Grouping}{

\code{\link{ResamplingVariableSizeTrainCV}} does not support grouping of observations.

}

\section{Hyper-parameters}{

  The number of cross-validation folds should be defined as the
  \code{fold} parameter.

  For each fold ID, the corresponding observations are considered the
  test set, and a variable number of other observations are considered
  the train set.

  The \code{random_seeds} parameter controls the number of random
  orderings of the train set that are considered.

  For each random order of the train set, the \code{min_train_data}
  parameter controls the smallest train set size considered.

  To determine the other train set sizes, we use an equally spaced grid
  on the log scale, from \code{min_train_data} to the largest train set
  size (all data not in test set). The
  number of train set sizes in this grid is determined by the
  \code{train_sizes} parameter.

}

\examples{
(var_sizes <- mlr3resampling::ResamplingVariableSizeTrainCV$new())
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
