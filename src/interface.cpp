#include <Rcpp.h>
#include "stratified_group_cv.h"

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
  return fold_vec;
}
