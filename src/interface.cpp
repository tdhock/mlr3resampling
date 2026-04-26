#include <Rcpp.h>
#include "stratified_group_cv.h"

// [[Rcpp::export]]
Rcpp::IntegerVector stratified_group_cv_interface
(Rcpp::IntegerVector strat_vec,
 Rcpp::IntegerVector group_vec,
 int num_folds
 ){
  int N_data = strat_vec.length();
  Rcpp::IntegerVector fold_vec(N_data);
  int status = stratified_group_cv
    (strat_vec.begin(), group_vec.begin(), N_data, num_folds,
     // inputs above, outputs below.
     fold_vec.begin());
  if(status==ERROR_STRATA_MUST_BE_NON_NEGATIVE)
    Rcpp::stop("strata must be non-negative");
  if(status==ERROR_GROUPS_MUST_BE_NON_NEGATIVE)
    Rcpp::stop("groups must be non-negative");
  if(status==ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX)
    Rcpp::stop("need at least one of each stratum from zero to max");
  if(status==ERROR_NEED_AT_LEAST_ONE_OF_EACH_GROUP_FROM_ZERO_TO_MAX)
    Rcpp::stop("need at least one of each group from zero to max");
  return fold_vec;
}
