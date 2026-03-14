# autoplot_cd.R
# Critical difference plot for benchmark results.
# Based on Demsar (2006) "Statistical Comparisons of Classifiers
# over Multiple Data Sets", JMLR 7, 1-30.

autoplot_cd <- function(bmr, meas, minimize = TRUE, p.value = 0.05) {
  if (!requireNamespace("PMCMRplus", quietly = TRUE))
    stop("PMCMRplus needed: install.packages('PMCMRplus')")

  # aggregate to one score per task/learner
  aggr <- bmr$aggregate(measures = mlr3::msr(meas))
  dt <- data.table::as.data.table(aggr)
  dt <- dt[, c("task_id", "learner_id", meas), with = FALSE]
  data.table::setnames(dt, meas, "score")

  # rank within each task; ties get average rank
  dt[, r := data.table::frank(
    if (minimize) score else -score,
    ties.method = "average"
  ), by = task_id]

  # mean rank per learner — lower is better
  rk <- dt[, .(mean_r = mean(r)), by = learner_id]
  data.table::setorder(rk, mean_r)

  k <- nrow(rk)
  N <- data.table::uniqueN(dt$task_id)

  # reshape to tasks x learners matrix for the tests
  wide <- data.table::dcast(dt, task_id ~ learner_id, value.var = "score")
  m <- as.matrix(wide[, -1, with = FALSE])
  rownames(m) <- wide$task_id
  # global friedman test — bail out with a message if not significant
  fp <- stats::friedman.test(m)$p.value
  if (fp >= p.value)
    message(sprintf(
      "Friedman test p = %.4f (>= %.2f): no significant differences found.",
      fp, p.value
    ))

  # critical difference threshold
  q  <- qtukey(1 - p.value, nmeans = k, df = Inf) / sqrt(2)
  cd <- q * sqrt(k * (k + 1) / (6 * N))

  # pairwise nemenyi post-hoc
  nh   <- PMCMRplus::frdAllPairsNemenyiTest(m)
  pmat <- nh$p.value
  # collect pairs that are NOT significantly different
  bars <- list()
  pmat_full <- as.matrix(nh$p.value)
  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      li <- rk$learner_id[i]
      lj <- rk$learner_id[j]
      pv <- NA
      if (li %in% rownames(pmat_full) && lj %in% colnames(pmat_full))
        pv <- pmat_full[li, lj]
      else if (lj %in% rownames(pmat_full) && li %in% colnames(pmat_full))
        pv <- pmat_full[lj, li]
      if (!is.na(pv) && pv >= p.value)
        bars <- c(bars, list(c(rk$mean_r[i], rk$mean_r[j])))
    }
  }

  # build the plot
  p <- ggplot2::ggplot(rk, ggplot2::aes(x = mean_r, y = 0)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = learner_id),
      vjust = -1.2, size = 3.5
    ) +
    ggplot2::scale_x_continuous(
      name   = sprintf("Average rank  (CD = %.2f)", cd),
      breaks = seq_len(k)
    ) +
    ggplot2::labs(title = "Critical Difference Plot") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank()
    )

  # add one horizontal bar per non-significant pair
  y_offset <- 0.3
  for (b in bars) {
    seg <- data.frame(x = min(b), xend = max(b), y = y_offset, yend = y_offset)
    p <- p + ggplot2::geom_segment(
      data = seg,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = 2, colour = "steelblue"
    )
    y_offset <- y_offset + 0.15
  }

  p
}