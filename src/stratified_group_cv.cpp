#include <armadillo>
#include "stratified_group_cv.h"

// from https://www.kaggle.com/code/jakubwasikowski/stratified-group-k-fold-cross-validation/notebook
int stratified_group_cv
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int N_fold,
 // inputs above, outputs below.
 int* fold_ptr){
  int strat_max = 0, group_max = 0;
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    int group = group_ptr[data_i];
    if(strat<0)return ERROR_STRATA_MUST_BE_POSITIVE;
    if(group<0)return ERROR_GROUPS_MUST_BE_POSITIVE;
    if(strat_max<strat)strat_max=strat;
    if(group_max<group)group_max=group;
  }
  int N_strat=strat_max+1, N_group=group_max+1;
  arma::vec strat_counts(N_strat, arma::fill::zeros);
  arma::mat
    strat_per_group_mat(N_strat, N_group, arma::fill::zeros),
    strat_per_fold_mat(N_strat, N_fold, arma::fill::zeros);
  arma::ivec fold_for_group(N_group);
  for(int data_i; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    int group = group_ptr[data_i];
    strat_per_group_mat(strat, group)++;
    strat_counts(strat)++;
  }
  arma::mat var_vec = arma::var(strat_per_group_mat, 0, 0);
  //std::cou t<< "var_vec is " << var_vec.n_rows << "x" << var_vec.n_cols << std::endl;
  arma::uvec sorted_groups = sort_index(var_vec, "descend");
  for(int group_i=0; group_i<N_group; group_i++){
    int group=sorted_groups(group_i);
    //std::cout << "group_i=" << group_i << "group=" << group << std::endl;      
    arma::vec group_vec = strat_per_group_mat.col(group);
    int best_fold=0;
    double min_eval=INFINITY;//, min_samples_in_fold=INFINITY;
    for(int fold=0; fold<N_fold; fold++){
      strat_per_fold_mat.col(fold) += group_vec;
      //std::cout << "strat_per_fold is " << strat_per_fold_mat.n_rows << "x" << strat_per_fold_mat.n_cols << std::endl;
      //std::cout << "strat_counts is " << strat_counts.n_rows << "x" << strat_counts.n_cols << std::endl;
      auto props = strat_per_fold_mat.each_col()/strat_counts;
      //std::cout << "props is " << props.n_rows << "x" << props.n_cols << std::endl;
      arma::mat sd_vec = arma::stddev(props, 0, 1);
      //std::cout << "sd_vec is " << sd_vec.n_rows << "x" << sd_vec.n_cols << std::endl;
      strat_per_fold_mat.col(fold) -= group_vec;
      double fold_eval = arma::mean(arma::mean(sd_vec));
      if(fold_eval<min_eval){
	min_eval=fold_eval;
	best_fold = fold;
      }
    }
    fold_for_group(group) = best_fold;
    strat_per_fold_mat.col(best_fold) += group_vec;
    //https://github.com/scikit-learn/scikit-learn/blob/fe2edb3cd/sklearn/model_selection/_split.py#L1086C1-L1106C25
  }
  for(int data_i=0; data_i<N_data; data_i++){
    int group = group_ptr[data_i];
    fold_ptr[data_i] = fold_for_group(group);
  }
  return 0;
}
