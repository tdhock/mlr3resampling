\name{ResamplingSameOtherSizesCV}
\alias{ResamplingSameOtherSizesCV}
\title{Resampling for comparing train subsets and sizes}
\description{
  \code{ResamplingSameOtherSizesCV}
  defines how a task is partitioned for
  resampling, for example in
  \code{\link[mlr3:resample]{resample()}} or
  \code{\link[mlr3:benchmark]{benchmark()}}.

  Resampling objects can be instantiated on a
  \code{\link[mlr3:Task]{Task}},
  which can use two new roles: \code{subset} and \code{fold}.
  
  After instantiation, sets can be accessed via
  \verb{$train_set(i)} and
  \verb{$test_set(i)}, respectively. 
}
\details{
  This is an implementation of SOAK, Same/Other/All K-fold cross-validation.
  A supervised learning algorithm inputs a train set, and outputs a
  prediction function, which can be used on a test set. If each data
  point belongs to a subset (such as geographic region, year, etc), then
  how do we know if it is possible to train on one subset, and predict
  accurately on another subset? Cross-validation can be used to determine
  the extent to which this is possible, by first assigning fold IDs from
  1 to K to all data (possibly using stratification, usually by subset
  and label). Then we loop over test sets (subset/fold combinations),
  train sets (same subset, other subsets, all subsets), and compute
  test/prediction accuracy for each combination.  Comparing
  test/prediction accuracy between same and other, we can determine the
  extent to which it is possible (perfect if same/other have similar
  test accuracy for each subset; other is usually somewhat less accurate
  than same; other can be just as bad as featureless baseline when the
  subsets have different patterns).

  This class has more parameters/potential applications than
  \code{\link{ResamplingSameOtherCV}} and
  \code{\link{ResamplingVariableSizeTrainCV}},
  which are older and should only be preferred
  for visualization purposes.
}

\section{Stratification}{
\code{ResamplingSameOtherSizesCV} supports stratified sampling.
The stratification variables are assumed to be discrete,
and must be stored in the \link[mlr3:Task]{Task} with column role
\code{stratum}.
If that role is not set, then all data are assigned to one stratum.
In case of multiple stratification variables,
each combination of the values of the stratification variables forms a
stratum.
When \code{fold} role is set, \code{stratum} role is not used for fold
assignment (folds are taken from \code{fold} column in that case).
When \code{sizes} param is at least 0, then downsampled train sets are
created, and if \code{stratum} role is set, we will attempt to preserve
strata proportions in downsampled train sets.
}

\section{Grouping}{
\code{ResamplingSameOtherSizesCV} supports grouping of
observations that will not be split in cross-validation.
The grouping variable is assumed to be discrete,
and must be stored in the \link[mlr3:Task]{Task} with column role
\code{group}.
If that role is not set, then each row is assigned to a different group.
When \code{fold} role is set, \code{group} role is not used for fold
assignment (folds are taken from \code{fold} column in that case).
When \code{sizes} param is at least 0, then downsampled train sets are
created, and if \code{group} role is set, we will keep groups together
in downsampled train sets.
}

\section{Subsets}{
\code{ResamplingSameOtherSizesCV} supports fixing a test subset,
then training on same/other/all subsets.
The subset variable is assumed to be discrete,
and must be stored in the \link[mlr3:Task]{Task} with column role \code{subset}.
}

\section{Parameters}{

The number of cross-validation folds K should be defined as the
\code{fold} parameter, default 3.

The number of random seeds for down-sampling should be defined as the
\code{seeds} parameter, default 1.

The ratio for down-sampling should be defined as the \code{ratio}
parameter, default 0.5. The min size of same and other sets is
repeatedly multiplied by this ratio, to obtain smaller sample sizes.

The number of down-sampling sizes/multiplications should be defined as
the \code{sizes} parameter, which can also take two special values:
default -1 means no down-sampling at all, and 0 means only down-sampling
to the sizes of the same/other sets.

The \code{ignore_subset} parameter should be either \code{TRUE} or
\code{FALSE} (default), whether to ignore the \code{subset}
role. \code{TRUE} only creates splits for same subset (even if task
defines \code{subset} role), and is useful for subtrain/validation
splits (hyper-parameter learning). Note that this feature will work on a
task with both \code{stratum} and \code{group} roles (unlike
\code{ResamplingCV}).

The \code{subsets} parameter should specify the train subsets of
interest: \code{"S"} (same),
\code{"O"} (other), \code{"A"} (all), \code{"SO"}, \code{"SA"},
\code{"SOA"} (default).

If \code{fold} column role is set, then that column will be used for
non-random fold assignment (see Reproducibility vignette).
Otherwise, the \code{group_stratum_algo} parameter controls the algorithm used for
random fold assignment.
We want to assign folds such that groups are not split (hard
constraint), and strata proportions are preserved (objective to optimize).
Computing an optimal fold assignment is NP-hard (discrete, need
to try all possible assignments of groups to folds to find best
solution).
Instead of computing the optimal solution (slow), we implement heuristic
algorithms (fast), which yield approximate solutions.
The choice of which heuristic is controlled by the
\code{group_stratum_algo} parameter, which can take these values:
\describe{
  \item{\code{"RSS"}}{Default, novel heuristic algorithm which attempts to
    minimize the residual sum of squares, between actual and ideal
    values of the strata/fold count matrix.
    First groups are sorted by RSS (group counts - ideal counts per
    stratum); ties are broken using number of samples in group,
    mean(ideal*actual) counts (mean over strata), random group order.
    Then each group is greedily assigned a fold in order; best fold
    results in min RSS, ties broken using total counts above ideal.
    For N data, K folds, G groups, and S strata, the algorithm is
    \code{O(N + K G S)} time and \code{O(N + S K)} memory.}
  \item{\code{"Wasikowski"}}{Same heuristic algorithm as in scikit-learn
    \code{StratifiedGroupKFold}, adapted from
    \url{https://www.kaggle.com/code/jakubwasikowski/stratified-group-k-fold-cross-validation}.
    First groups are sorted by standard deviation over strata counts;
    ties are broken by random group order.
    Then each group is greedily assigned a fold in order; best fold results in the
    smallest mean SD of the strata/fold count matrix (SD over folds is
    computed for each stratum, then mean of SDs, minimized over folds),
    ties broken using number of samples in fold.
    For N data, K folds, G groups, and S strata, the algorithm is
    \code{O(N + K^2 G S)} time and \code{O(N + S K + S G)} memory (could be problematic when G and
    S are both large).}
  \item{\code{"WasikowskiLinearMemory"}}{Same logic and time complexity
    but only \code{O(N + S K)} memory.}
}

The train/test splits are defined by all possible combinations of
test subset, test fold, train subsets (same/other/all), down-sampling
sizes, and random seeds.
The splits are stored in
\verb{$instance$iteration.dt}.

}

\examples{
same_other_sizes <- mlr3resampling::ResamplingSameOtherSizesCV$new()
same_other_sizes$param_set$values$folds <- 5
}
\seealso{
  \itemize{
    \item arXiv paper \url{https://arxiv.org/abs/2410.08643} describing
    SOAK algorithm.
    \item Articles
    \url{https://github.com/tdhock/mlr3resampling/wiki/Articles}
    \item Package \CRANpkg{mlr3} for standard
    \code{\link[mlr3:Resampling]{Resampling}}, which does not support comparing
    train on Same/Other/All subsets.
    \item \code{vignette(package="mlr3resampling")} for more detailed examples.
  }
}
\concept{Resampling}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ResamplingSameOtherSizesCV-new}{\code{Resampling$new()}}
\item \href{#method-ResamplingSameOtherSizesCV-train_set}{\code{Resampling$train_set()}}
\item \href{#method-ResamplingSameOtherSizesCV-test_set}{\code{Resampling$test_set()}}
}
}

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ResamplingSameOtherSizesCV-new"></a>}}
\if{latex}{\out{\hypertarget{method-ResamplingSameOtherSizesCV-new}{}}}
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
}

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
\if{html}{\out{<a id="method-ResamplingSameOtherSizesCV-train_set"></a>}}
\if{latex}{\out{\hypertarget{method-ResamplingSameOtherSizesCV-train_set}{}}}
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
\if{html}{\out{<a id="method-ResamplingSameOtherSizesCV-test_set"></a>}}
\if{latex}{\out{\hypertarget{method-ResamplingSameOtherSizesCV-test_set}{}}}
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
