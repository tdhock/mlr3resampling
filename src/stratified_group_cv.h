#define ERROR_STRATA_MUST_BE_POSITIVE 1
#define ERROR_GROUPS_MUST_BE_POSITIVE 2

int stratified_group_cv
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int num_folds,
 // inputs above, outputs below.
 int* fold_ptr);
