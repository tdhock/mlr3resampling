library(testthat)
library(data.table)

test_that("pvalue_compute supports pvalue-style grouping", {
  levs <- c("all", "all-same", "same", "other-same", "other", "")
  score_value <- data.table(
    task_id="taskA",
    test.subset="subsetA",
    algorithm="featureless",
    test.fold=rep(1:2, each=3),
    train.subsets=rep(c("same", "other", "all"), times=2),
    value=c(0.50, 0.80, 0.55, 0.60, 0.82, 0.58)
  )[, Train_subsets := factor(train.subsets, levs)][]

  out <- mlr3resampling:::pvalue_compute(
    score_value=score_value,
    measure.vars=c("other", "all"),
    cast_id_cols=c("task_id", "test.subset", "algorithm", "test.fold"),
    melt_id_cols=c("task_id", "test.subset", "algorithm", "test.fold", "same"),
    stats_by=c("task_id", "test.subset", "algorithm", "Train_subsets"),
    range_by=c("task_id", "test.subset"),
    pvalue_by=c("task_id", "test.subset", "algorithm", "Train_subsets"),
    pvalue_train_subset_levels=levs,
    show_n_full_in_stats_label=FALSE
  )

  expect_true(all(c("stats", "pvalues") %in% names(out)))
  expect_true(all(c("task_id", "test.subset", "algorithm", "Train_subsets") %in% names(out$stats)))
  expect_true(all(c("task_id", "test.subset", "algorithm", "Train_subsets") %in% names(out$pvalues)))
  expect_true(all(grepl("^P = |^P <", out$pvalues$text_label)))
})

test_that("pvalue_compute supports downsample-style grouping and labels", {
  label_order <- c("other", "other-same", "same", "all-same", "all")
  score_value <- CJ(
    sample_size=factor(c("full", "10"), c("full", "10")),
    test.fold=1:2,
    train.subsets=c("same", "other", "all")
  )[, let(
    value=fcase(
      train.subsets == "same", 0.42,
      train.subsets == "other", 0.40,
      default=0.39
    ) + ifelse(sample_size == "full", 0, 0.01) + test.fold / 100,
    n.train.groups=ifelse(sample_size == "full", 20L, 10L),
    Train_subsets=factor(train.subsets, label_order)
  )][]

  out <- mlr3resampling:::pvalue_compute(
    score_value=score_value,
    measure.vars=c("other", "all"),
    cast_id_cols=c("sample_size", "test.fold"),
    melt_id_cols=c("sample_size", "test.fold", "same"),
    stats_by=c("sample_size", "Train_subsets"),
    range_by="sample_size",
    pvalue_by=c("sample_size", "Train_subsets"),
    pvalue_train_subset_levels=label_order,
    add_n_train_from="n.train.groups",
    show_n_full_in_stats_label=TRUE
  )

  expect_true(all(c("full", "10") %in% as.character(unique(out$stats$sample_size))))
  expect_true(any(grepl(", N = [0-9]+$", out$stats[sample_size == "full", text_label])))
  expect_true(all(!grepl(", N = [0-9]+$", out$stats[sample_size == "10", text_label])))
  expect_true(all(grepl("^P = |^P <", out$pvalues$text_label)))
})
