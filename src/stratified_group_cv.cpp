#include <armadillo>
#include "stratified_group_cv.h"
#define ABS(x)((x)<0 ? (-(x)) : (x))
#define CLOSE(x,y)(ABS((x)-(y))<1e-3)

// adapted from
// https://www.Wasikowski.com/code/jakubwasikowski/stratified-group-k-fold-cross-validation/notebook
// https://github.com/scikit-learn/scikit-learn/blob/fe2edb3cd/sklearn/model_selection/_split.py#L1086C1-L1106C25
int stratified_group_cv_Wasikowski
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
    if(strat<0)return ERROR_STRATA_MUST_BE_NON_NEGATIVE;
    if(group<0)return ERROR_GROUPS_MUST_BE_NON_NEGATIVE;
    if(strat_max<strat)strat_max=strat;
    if(group_max<group)group_max=group;
  }
  int N_strat=strat_max+1, N_group=group_max+1;
  arma::vec
    group_counts(N_group, arma::fill::zeros),
    strat_counts(N_strat, arma::fill::zeros),
    group_vec(N_strat);
  arma::mat
    var_vec(N_group,1),
    sd_vec(N_strat,1),
    props(N_strat, N_fold),
    strat_per_group_mat(N_strat, N_group, arma::fill::zeros),
    strat_per_fold_mat(N_strat, N_fold, arma::fill::zeros);
  arma::ivec fold_for_group(N_group);
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    int group = group_ptr[data_i];
    strat_per_group_mat(strat, group)++;
    strat_counts(strat)++;
    group_counts(group)++;
  }
  for(int group=0; group<N_group; group++){
    if(group_counts(group)==0)return ERROR_NEED_AT_LEAST_ONE_OF_EACH_GROUP_FROM_ZERO_TO_MAX;
  }
  for(int strat=0; strat<N_strat; strat++){
    if(strat_counts(strat)==0)return ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX;
  }
  var_vec = arma::var(strat_per_group_mat, 0, 0);
  arma::uvec sorted_groups = sort_index(var_vec, "descend");
  // todo how to tie break using mean?
  for(int group_i=0; group_i<N_group; group_i++){
    int group=sorted_groups(group_i);
    //std::cout << "group_i=" << group_i << "group=" << group << std::endl;      
    group_vec = strat_per_group_mat.col(group);
    int best_fold=0;
    double min_eval=INFINITY;//, min_samples_in_fold=INFINITY;
    for(int fold=0; fold<N_fold; fold++){
      strat_per_fold_mat.col(fold) += group_vec;
      props = strat_per_fold_mat.each_col()/strat_counts;
      sd_vec = arma::stddev(props, 0, 1);
      strat_per_fold_mat.col(fold) -= group_vec;
      double fold_eval = arma::mean(arma::mean(sd_vec));
      if(fold_eval<min_eval){
	min_eval=fold_eval;
	best_fold = fold;
      }
    }
    fold_for_group(group) = best_fold;
    strat_per_fold_mat.col(best_fold) += group_vec;
  }
  for(int data_i=0; data_i<N_data; data_i++){
    int group = group_ptr[data_i];
    fold_ptr[data_i] = fold_for_group(group);
  }
  return 0;
}

int stratified_group_cv_WasikowskiLinearMemory
(const int* strat_ptr, // in 0,…,strat_max
 const int* group_ptr, // sorted, non-decreasing.
 const int N_data,
 const int N_fold,
 // inputs above, outputs below.
 int* fold_ptr){
  int strat_max = 0;
  // begin by scanning all data, checking for errors and determining
  // number of strata.
  for(int data_i=0; data_i<N_data; data_i++){
    if(0<data_i && group_ptr[data_i] < group_ptr[data_i-1])
      return ERROR_GROUP_MUST_BE_NON_DECREASING;
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
  // count each stratum, error if any are zero.
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    strat_counts(strat)++;
    fold_ptr[data_i] = -1;
  }
  for(int strat=0; strat<N_strat; strat++){
    if(strat_counts(strat)==0)return ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX;
  }
  // main fold assignment loop over data, already sorted by group.
  int data_i_at_group_start;
  for(int data_i=0; data_i<N_data; data_i++){
    int group = group_ptr[data_i];
    if(data_i==0 || (data_i>0 && group_ptr[data_i-1] != group)){
      // start of a group, so restart counts to zero.
      data_i_at_group_start=data_i;
      strat_counts_for_group.zeros();
    }
    // add to counts for this stratum.
    int strat = strat_ptr[data_i];
    strat_counts_for_group(strat)++;
    if(data_i==N_data-1 || (data_i+1<N_data && group_ptr[data_i+1] != group)){
      // end of a group, so use counts to determine optimal fold.
      int best_fold=0;
      double min_sd=INFINITY;
      for(int fold=0; fold<N_fold; fold++){
        strat_per_fold_mat.col(fold) += strat_counts_for_group;
        props = strat_per_fold_mat.each_col()/strat_counts;
        // sd_vec has one element per stratum;
        // each is the variability between folds.
        sd_vec = arma::stddev(props, 0, 1);
        strat_per_fold_mat.col(fold) -= strat_counts_for_group;
        double fold_sd = arma::mean(arma::mean(sd_vec));
        if(fold_sd<min_sd){
          // best fold results in the least variability between folds,
          // averaged over all strata.
          min_sd=fold_sd;
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

int stratified_group_cv_RSS
(const int* strat_ptr, // in 0,…,strat_max
 const int* group_ptr, // sorted, non-decreasing.
 const int N_data,
 const int N_fold,
 // inputs above, outputs below.
 int* fold_ptr){
  int strat_max = 0;
  // begin by scanning all data, checking for errors and determining
  // number of strata.
  for(int data_i=0; data_i<N_data; data_i++){
    if(0<data_i && group_ptr[data_i] < group_ptr[data_i-1])
      return ERROR_GROUP_MUST_BE_NON_DECREASING;
    int strat = strat_ptr[data_i];
    if(strat<0)return ERROR_STRATA_MUST_BE_NON_NEGATIVE;
    if(strat_max<strat)strat_max=strat;
  }
  int N_strat=strat_max+1;
  arma::vec
    strat_counts(N_strat, arma::fill::zeros),
    ideal_strat_counts_per_fold(N_strat),
    strat_counts_for_group(N_strat);
  arma::mat
    strat_per_fold_mat(N_strat, N_fold, arma::fill::zeros);
  // count each stratum, error if any are zero.
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    strat_counts(strat)++;
    fold_ptr[data_i] = -1;
  }
  for(int strat=0; strat<N_strat; strat++){
    if(strat_counts(strat)==0)return ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX;
  }
  ideal_strat_counts_per_fold = strat_counts / N_fold;
  double RSS = 0;
  for(int strat=0; strat<N_strat; strat++){
    RSS += N_fold*ideal_strat_counts_per_fold(strat);
  }
  // main fold assignment loop over data, already sorted by group.
  int data_i_at_group_start;
  for(int data_i=0; data_i<N_data; data_i++){
    int group = group_ptr[data_i];
    if(data_i==0 || (data_i>0 && group_ptr[data_i-1] != group)){
      // start of a group, so restart counts to zero.
      data_i_at_group_start=data_i;
      strat_counts_for_group.zeros();
    }
    // add to counts for this stratum.
    int strat_i = strat_ptr[data_i];
    strat_counts_for_group(strat_i)++;
    if(data_i==N_data-1 || (data_i+1<N_data && group_ptr[data_i+1] != group)){
      // end of a group, so use counts to determine optimal fold.
      double best_rss_update=INFINITY;
      int best_fold=0;
      double best_over_ideal=0;
      for(int fold=0; fold<N_fold; fold++){
	double fold_over_ideal=0;
	double fold_rss_update=0;
	for(int strat=0; strat<N_strat; strat++){
	  double n_hat = strat_per_fold_mat(strat, fold);
	  double diff = n_hat-ideal_strat_counts_per_fold(strat);
	  double n_group = strat_counts_for_group(strat);
	  double maybe_over = n_group + diff;
	  if(maybe_over>0)fold_over_ideal += maybe_over;
	  fold_rss_update += (diff*2+n_group)*n_group;
	}
	if(fold_rss_update<best_rss_update ||
	   (CLOSE(fold_rss_update,best_rss_update) && fold_over_ideal<best_over_ideal)
	   ){
	  best_fold=fold;
	  best_rss_update = fold_rss_update;
	  best_over_ideal = fold_over_ideal;
	}
      }
      for(int set_i=data_i_at_group_start; set_i<=data_i; set_i++){
        fold_ptr[set_i] = best_fold;
      }
      strat_per_fold_mat.col(best_fold) += strat_counts_for_group;
      RSS += best_rss_update;
    }
  }
  return 0;
}
