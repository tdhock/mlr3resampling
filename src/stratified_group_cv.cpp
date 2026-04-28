#include <armadillo>
#include "stratified_group_cv.h"

// adapted from
// https://www.kaggle.com/code/jakubwasikowski/stratified-group-k-fold-cross-validation/notebook
// https://github.com/scikit-learn/scikit-learn/blob/fe2edb3cd/sklearn/model_selection/_split.py#L1086C1-L1106C25
int stratified_group_cv
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int N_fold,
 // inputs above, outputs below.
 int* fold_ptr){
  for(int data_i=1; data_i<N_data; data_i++){
    if(group_ptr[data_i] < group_ptr[data_i-1])
      return ERROR_GROUP_MUST_BE_NON_DECREASING;
  }
  int strat_max = 0;
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    if(strat<0)return ERROR_STRATA_MUST_BE_NON_NEGATIVE;
    if(strat_max<strat)strat_max=strat;
  }
  int N_strat=strat_max+1;
  arma::vec
    strat_counts(N_strat, arma::fill::zeros),
    strat_counts_for_group(N_strat);
  arma::mat
    sd_vec(N_strat,1),
    props(N_strat, N_fold),
    strat_per_fold_mat(N_strat, N_fold, arma::fill::zeros);
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    strat_counts(strat)++;
    fold_ptr[data_i] = -1;
  }
  for(int strat=0; strat<N_strat; strat++){
    if(strat_counts(strat)==0)return ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX;
  }
  int data_i_at_group_start;
  for(int data_i=0; data_i<N_data; data_i++){
    int group = group_ptr[data_i];
    if(data_i==0 || (data_i>0 && group_ptr[data_i-1] != group)){
      data_i_at_group_start=data_i;
      strat_counts_for_group.zeros();
    }
    int strat = strat_ptr[data_i];
    strat_counts_for_group(strat)++;
    if(data_i==N_data-1 || (data_i+1<N_data && group_ptr[data_i+1] != group)){
      int best_fold=0;
      double min_eval=INFINITY;
      for(int fold=0; fold<N_fold; fold++){
	strat_per_fold_mat.col(fold) += strat_counts_for_group;
	props = strat_per_fold_mat.each_col()/strat_counts;
	sd_vec = arma::stddev(props, 0, 1);
	strat_per_fold_mat.col(fold) -= strat_counts_for_group;
	double fold_eval = arma::mean(arma::mean(sd_vec));
	if(fold_eval<min_eval){
	  min_eval=fold_eval;
	  best_fold = fold;
	}
      }
      for(int set_i=data_i_at_group_start; set_i<=data_i; set_i++){
	fold_ptr[set_i] = best_fold;
      }
      strat_per_fold_mat.col(best_fold) += strat_counts_for_group;
    }
  }
  return 0;
}
