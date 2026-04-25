#include "stratified_group_cv.h"
#include <vector>

// from https://www.kaggle.com/code/jakubwasikowski/stratified-group-k-fold-cross-validation/notebook
int stratified_group_cv
(const int* strat_ptr,
 const int* group_ptr,
 const int N_data,
 const int num_folds,
 // inputs above, outputs below.
 int* fold_ptr){
  int strat_max = 0, group_max = 0;
  for(int data_i=0; data_i<N_data; data_i++){
    int strat = strat_ptr[data_i];
    int group = group_ptr[data_i];
    if(strat<0)return ERROR_STRATA_MUST_BE_POSITIVE;
    if(group<0)return ERROR_GROUPS_MUST_BE_POSITIVE;
    if(strat_max<strat)strat_max=strat;
    if(group_max<strat)group_max=group;
  }
  int N_strat=strat_max+1, N_group=group_max+1;
  std::vector<int> strat_counts(N_strat), strat_per_group_mat(N_strat*N_group);
//     labels_num = np.max(y) + 1
//     y_counts_per_group = defaultdict(lambda: np.zeros(labels_num))
//     y_distr = Counter()
//     for label, g in zip(y, groups):
//         y_counts_per_group[g][label] += 1
//         y_distr[label] += 1
//     y_counts_per_fold = defaultdict(lambda: np.zeros(labels_num))
//     groups_per_fold = defaultdict(set)
//     def eval_y_counts_per_fold(y_counts, fold):
//         y_counts_per_fold[fold] += y_counts
//         std_per_label = []
//         for label in range(labels_num):
//             label_std = np.std([y_counts_per_fold[i][label] / y_distr[label] for i in range(k)])
//             std_per_label.append(label_std)
//         y_counts_per_fold[fold] -= y_counts
//         return np.mean(std_per_label)
//     groups_and_y_counts = list(y_counts_per_group.items())
//     random.Random(seed).shuffle(groups_and_y_counts)
// (Pdb) sorted(groups_and_y_counts, key=lambda x: -np.std(x[1]))[:4]
// [('fa90fa5b1ee11c86938398b60abc32cb', array([ 12.,  44., 173., 223.,   7.])), ('c00756f2bdd8fa88fc9f07a8309f7d5d', array([ 1., 29., 99., 86., 16.])), ('aa66486163b6cbc25ea62a34b11c9b91', array([  1.,  46.,  93., 112.,  63.])), ('b53c34474d9e24574bcec6a3d3306a0d', array([ 4., 52., 76., 86., 10.]))]
// (Pdb) sorted(groups_and_y_counts, key=lambda x: -np.std(x[1]))[-4:]
// [('d98e51540d2b9dc4e36fa6d68b4e6649', array([0., 0., 0., 0., 1.])), ('d24f39ef36b742422473fc1218caf41f', array([0., 0., 0., 0., 1.])), ('27083c7f0a040994ca84395f4b17d80b', array([2., 2., 3., 2., 2.])), ('32856657cf2e52ef734ef4e93f460033', array([2., 2., 3., 2., 2.]))]
//     for g, y_counts in sorted(groups_and_y_counts, key=lambda x: -np.std(x[1])):
//         best_fold = None
//         min_eval = None
//         for i in range(k):
//             fold_eval = eval_y_counts_per_fold(y_counts, i)
//             if min_eval is None or fold_eval < min_eval:
//                 min_eval = fold_eval
//                 best_fold = i
//         y_counts_per_fold[best_fold] += y_counts
//         groups_per_fold[best_fold].add(g)
  return 0;
}
