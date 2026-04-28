#include <Rcpp.h>
#include "stratified_group_cv.h"

// [[Rcpp::export]]
Rcpp::IntegerVector stratified_group_cv_interface
(const Rcpp::IntegerVector strat_vec,
 const Rcpp::IntegerVector group_vec,
 const int num_folds
 ){
  int N_data = strat_vec.length();
  Rcpp::IntegerVector fold_vec(N_data);
  int status = stratified_group_cv
    (strat_vec.begin(), group_vec.begin(), N_data, num_folds,
     // inputs above, outputs below.
     fold_vec.begin());
  if(status==ERROR_STRATA_MUST_BE_NON_NEGATIVE)
    Rcpp::stop("strata must be non-negative");
  if(status==ERROR_GROUP_MUST_BE_NON_DECREASING)
    Rcpp::stop("group must be non-decreasing");
  if(status==ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX)
    Rcpp::stop("need at least one of each stratum from zero to max");
  return fold_vec;
}
