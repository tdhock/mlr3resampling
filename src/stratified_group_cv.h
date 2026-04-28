#define ERROR_STRATA_MUST_BE_NON_NEGATIVE 1
#define ERROR_GROUPS_MUST_BE_NON_NEGATIVE 2
#define ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX 3
#define ERROR_NEED_AT_LEAST_ONE_OF_EACH_GROUP_FROM_ZERO_TO_MAX 4
#define ERROR_GROUP_MUST_BE_NON_DECREASING 5

int stratified_group_cv_kaggle
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int num_folds,
 // inputs above, outputs below.
 int* fold_ptr);

int stratified_group_cv_new
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int num_folds,
 // inputs above, outputs below.
 int* fold_ptr);
