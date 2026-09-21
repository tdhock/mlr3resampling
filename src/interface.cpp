#include <Rcpp.h>
#include "stratified_group_cv.h"

// [[Rcpp::export]]
Rcpp::IntegerVector stratified_group_cv_Wasikowski_interface
(const Rcpp::IntegerVector strat_vec,
 const Rcpp::IntegerVector group_vec,
 const int num_folds
 ){
  int N_data = strat_vec.length();
  Rcpp::IntegerVector fold_vec(N_data);
  int status = stratified_group_cv_Wasikowski
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

// [[Rcpp::export]]
Rcpp::IntegerVector stratified_group_cv_WasikowskiLimitedMemory_interface
(Rcpp::IntegerVector strat_vec,
 Rcpp::IntegerVector group_vec,
 int num_folds
 ){
  int N_data = strat_vec.length();
  Rcpp::IntegerVector fold_vec(N_data);
  int status = stratified_group_cv_WasikowskiLimitedMemory
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

// [[Rcpp::export]]
Rcpp::IntegerVector stratified_group_cv_RSS_interface
(const Rcpp::IntegerVector strat_vec,
 const Rcpp::IntegerVector group_vec,
 const int num_folds
 ){
  int N_data = strat_vec.length();
  Rcpp::IntegerVector fold_vec(N_data);
  int status = stratified_group_cv_RSS
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

// [[Rcpp::export]]
void set_RSS_stats_interface
(const Rcpp::IntegerVector strat_vec,
 const Rcpp::IntegerVector group_vec,
 const Rcpp::IntegerVector random_order_vec,
 const int num_folds,
 Rcpp::NumericVector rss_vec,
 Rcpp::NumericVector neg_nrow_vec,
 Rcpp::NumericVector Wsum_vec,
 Rcpp::NumericVector g_ord_vec
 ){
  int N_data = strat_vec.length();
  int status = set_RSS_stats
    (strat_vec.begin(), group_vec.begin(), random_order_vec.begin(), N_data, num_folds,
     // inputs above, outputs below.
     rss_vec.begin(), neg_nrow_vec.begin(), Wsum_vec.begin(), g_ord_vec.begin());
  if(status==ERROR_STRATA_MUST_BE_NON_NEGATIVE)
    Rcpp::stop("strata must be non-negative");
  if(status==ERROR_GROUP_MUST_BE_NON_DECREASING)
    Rcpp::stop("group must be non-decreasing");
  if(status==ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX)
    Rcpp::stop("need at least one of each stratum from zero to max");
}
