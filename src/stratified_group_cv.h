#define ERROR_STRATA_MUST_BE_NON_NEGATIVE 11
#define ERROR_GROUPS_MUST_BE_NON_NEGATIVE 12
#define ERROR_NEED_AT_LEAST_ONE_OF_EACH_STRATUM_FROM_ZERO_TO_MAX 21
#define ERROR_NEED_AT_LEAST_ONE_OF_EACH_GROUP_FROM_ZERO_TO_MAX 22
#define ERROR_GROUP_MUST_BE_NON_DECREASING 31

int stratified_group_cv_Wasikowski
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int num_folds,
 // inputs above, outputs below.
 int* fold_ptr);

int stratified_group_cv_WasikowskiLimitedMemory
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int num_folds,
 // inputs above, outputs below.
 int* fold_ptr);

int stratified_group_cv_RSS
(const int* strat_ptr, // in 0,…,strat_max
 const int* group_ptr, // sorted, non-decreasing.
 const int N_data,
 const int N_fold,
 // inputs above, outputs below.
 int* fold_ptr);
