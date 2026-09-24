#' Read out an A/B/n test on a conversion rate
#'
#' Compares one or more treatment arms against a control arm. Returns rates,
#' absolute and relative lifts with confidence intervals, raw and
#' multiplicity-adjusted p-values, an optional sample ratio mismatch (SRM)
#' check, a check on whether the test was big enough, and a plain-English
#' summary.
#'
#' @param data A data frame. Either one row per user (with a 0/1 outcome) or
#'   one row per arm (with counts, see `trials`).
#' @param group Name of the column holding the arm labels.
#' @param outcome Name of the outcome column: 0/1 (or TRUE/FALSE) per user,
#'   or the number of conversions per arm when `trials` is supplied.
#' @param ctrl The label of the control arm.
#' @param treatment Label(s) of the treatment arm(s). `NULL` (default) uses
#'   every arm that isn't control.
#' @param trials For aggregated data: name of the column holding the number
#'   of users/sends per arm. Leave `NULL` for row-level data.
#' @param conf_level Confidence level for intervals and significance.
#' @param adjust Multiplicity correction, any method accepted by
#'   [stats::p.adjust()]: `"holm"`, `"bonferroni"`, `"BH"`, `"none"`, ...
#' @param alternative `"two.sided"`, `"greater"` or `"less"`, where
#'   `"greater"` means treatment > control.
#' @param mde Optional target minimum detectable effect. If supplied, the
#'   readout checks whether each arm reached the required sample size.
#' @param mde_type `"absolute"` or `"relative"`; see [caples_design()].
#' @param power Target power used for the sample-size check and achieved MDE.
#' @param allocation Optional intended traffic split. Either named by arm
#'   label or ordered control first, then treatments. Enables the SRM check.
#' @param srm_threshold p-value below which SRM is flagged. The usual
#'   convention is 0.001.
#' @param design Optional plan from [caples_design()], made before the test.
#'   When supplied, `mde`, `mde_type`, `power`, `conf_level`, `alternative`
#'   and (if not given) `allocation` are taken from the plan, and the
#'   sample-size check compares each arm with the planned n. This is the
#'   recommended workflow: decide first, analyse second.
#'
#' @details Confidence intervals are two-sided and are not adjusted for
#'   multiple comparisons; only p-values are adjusted. The test is a pooled
#'   two-proportion z-test, equivalent to `prop.test(correct = FALSE)`.
#'
#' @return An object of class `caples_test`.
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   arm = rep(c("control", "A", "B"), each = 5000),
#'   converted = c(rbinom(5000, 1, 0.050),
#'                 rbinom(5000, 1, 0.056),
#'                 rbinom(5000, 1, 0.062))
#' )
#' # Recommended: plan first, then pass the plan to the readout
#' plan <- caples_design(baseline = 0.05, mde = 0.01, power = 0.85, arms = 3)
#' caples_test(df, group = "arm", outcome = "converted", ctrl = "control",
#'            design = plan)
#'
#' # Without a plan: supply the targets directly
#' caples_test(df, group = "arm", outcome = "converted", ctrl = "control",
#'            mde = 0.01, power = 0.85, allocation = c(1, 1, 1))
caples_test <- function(data,
                       group,
                       outcome,
                       ctrl,
                       treatment = NULL,
                       trials = NULL,
                       conf_level = 0.95,
                       adjust = "holm",
                       alternative = c("two.sided", "greater", "less"),
                       mde = NULL,
                       mde_type = c("absolute", "relative"),
                       power = 0.8,
                       allocation = NULL,
                       srm_threshold = 0.001,
                       design = NULL) {
  # A plan made before the test takes precedence over direct arguments.
  # This must run before anything reassigns the arguments, or missing()
  # stops working.
  if (!is.null(design)) {
    if (!inherits(design, "caples_design")) {
      stop("`design` must be the output of caples_design().", call. = FALSE)
    }
    passed <- c(mde = !is.null(mde), mde_type = !missing(mde_type),
                power = !missing(power), conf_level = !missing(conf_level),
                alternative = !missing(alternative))
    if (any(passed)) {
      warning("Using ", paste(names(passed)[passed], collapse = ", "),
              " from `design`; the values passed directly were ignored.",
              call. = FALSE)
    }
    mde         <- design$mde
    mde_type    <- design$mde_type
    power       <- design$power
    conf_level  <- design$conf_level
    alternative <- design$alternative
    if (is.null(allocation)) allocation <- design$allocation
  }

  alternative <- match.arg(alternative)
  mde_type    <- match.arg(mde_type)
  check_prob(conf_level, "conf_level")
  check_prob(power, "power")
  if (!adjust %in% stats::p.adjust.methods) {
    stop("`adjust` must be one of: ",
         paste(stats::p.adjust.methods, collapse = ", "), call. = FALSE)
  }

  # ---- 1. Validate and count ----------------------------------------------
  counts <- count_arms(data, group, outcome, trials)

  if (!ctrl %in% counts$arm) {
    stop(sprintf("Control arm '%s' not found in column `%s`.", ctrl, group),
         call. = FALSE)
  }
  if (is.null(treatment)) treatment <- setdiff(counts$arm, ctrl)
  if (length(treatment) == 0) stop("No treatment arms found.", call. = FALSE)
  bad <- setdiff(treatment, counts$arm)
  if (length(bad) > 0) {
    stop("Treatment arm(s) not found: ", paste(bad, collapse = ", "),
         call. = FALSE)
  }

  arms <- c(ctrl, treatment)
  if (!is.null(design) && length(design$n_per_arm) != length(arms)) {
    stop(sprintf("`design` was planned for %d arms but the data has %d.",
                 length(design$n_per_arm), length(arms)), call. = FALSE)
  }
  counts <- counts[match(arms, counts$arm), ]
  rownames(counts) <- NULL
  counts$rate <- counts$conversions / counts$n

  alpha <- 1 - conf_level
  z_ci  <- stats::qnorm(1 - alpha / 2)
  m     <- length(treatment)
  alpha_sizing <- if (adjust != "none" && m > 1) alpha / m else alpha
  if (!is.null(design)) {
    alpha_sizing <- design$alpha_sizing
    if (!design$adjust_sizing && adjust != "none" && m > 1) {
      warning("The plan was sized without a multiplicity adjustment, but ",
              "`adjust = \"", adjust, "\"` is being applied. The test may ",
              "be underpowered.", call. = FALSE)
    }
  }

  # ---- 2. Compare each treatment with control -----------------------------
  n_c <- counts$n[1]
  x_c <- counts$conversions[1]
  p_c <- counts$rate[1]

  results <- do.call(rbind, lapply(treatment, function(arm) {
    row <- counts[counts$arm == arm, ]
    compare_arm(arm, row$n, row$conversions, n_c, x_c, z_ci, alternative)
  }))
  results$p_adjusted  <- stats::p.adjust(results$p_value, method = adjust)
  results$significant <- results$p_adjusted < alpha

  # Smallest lift each comparison could reliably detect.
  results$achieved_mde_abs <- vapply(treatment, function(arm) {
    achieved_mde(p_c, n_c, counts$n[counts$arm == arm], alpha_sizing, power,
                 alternative)
  }, numeric(1), USE.NAMES = FALSE)
  results$achieved_mde_rel <- results$achieved_mde_abs / p_c
  rownames(results) <- NULL

  # ---- 3. Sample ratio mismatch -------------------------------------------
  srm <- NULL
  alloc <- NULL
  if (!is.null(allocation)) {
    alloc <- resolve_allocation(allocation, length(arms), labels = arms)
    chi <- suppressWarnings(stats::chisq.test(counts$n, p = alloc))
    srm <- list(
      expected_share = stats::setNames(alloc, arms),
      observed_share = stats::setNames(counts$n / sum(counts$n), arms),
      p_value = chi$p.value,
      passed  = chi$p.value >= srm_threshold,
      threshold = srm_threshold
    )
  }

  # ---- 4. Was the test big enough? ----------------------------------------
  sizing <- NULL
  actual <- stats::setNames(counts$n, arms)

  if (!is.null(design)) {
    # Compare against the plan made before the test.
    required <- stats::setNames(unname(design$n_per_arm), arms)
    sizing <- list(source = "plan", required = required, actual = actual,
                   enough = all(actual >= required), mde = mde,
                   mde_type = mde_type, planned_baseline = design$baseline,
                   observed_baseline = p_c)
  } else if (!is.null(mde)) {
    # No plan: recalculate from the observed control rate.
    design_alloc <- if (!is.null(alloc)) alloc else counts$n / sum(counts$n)
    recalc <- tryCatch(
      caples_design(baseline = p_c, mde = mde, mde_type = mde_type,
                   power = power, arms = length(arms),
                   allocation = design_alloc, conf_level = conf_level,
                   alternative = alternative,
                   adjust_sizing = adjust != "none"),
      error = function(e) NULL
    )
    if (!is.null(recalc)) {
      required <- stats::setNames(unname(recalc$n_per_arm), arms)
      sizing <- list(source = "recalculated", required = required,
                     actual = actual, enough = all(actual >= required),
                     mde = mde, mde_type = mde_type,
                     planned_baseline = NA_real_, observed_baseline = p_c)
    }
  }

  out <- list(
    arms        = counts,
    results     = results,
    srm         = srm,
    sizing      = sizing,
    settings    = list(ctrl = ctrl, conf_level = conf_level, adjust = adjust,
                       alternative = alternative, power = power,
                       n_comparisons = m)
  )
  out$summary <- build_summary(out)
  class(out) <- "caples_test"
  out
}


# ---- Internal helpers -------------------------------------------------------

count_arms <- function(data, group, outcome, trials) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  needed <- c(group, outcome, trials)
  missing <- setdiff(needed, names(data))
  if (length(missing) > 0) {
    stop("Column(s) not found in `data`: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  labels <- as.character(data[[group]])
  arm_order <- if (is.factor(data[[group]])) levels(data[[group]]) else unique(labels)

  if (is.null(trials)) {
    y <- data[[outcome]]
    if (is.logical(y)) y <- as.integer(y)
    if (anyNA(y) || anyNA(labels)) {
      keep <- !is.na(y) & !is.na(labels)
      warning(sprintf("Dropped %d row(s) with missing group or outcome.",
                      sum(!keep)), call. = FALSE)
      y <- y[keep]
      labels <- labels[keep]
    }
    if (!all(y %in% c(0, 1))) {
      stop("`outcome` must be 0/1 or TRUE/FALSE for row-level data. ",
           "For counts per arm, supply `trials`.", call. = FALSE)
    }
    n <- tapply(y, labels, length)
    x <- tapply(y, labels, sum)
    arm_order <- intersect(arm_order, names(n))
    data.frame(arm = arm_order,
               n = as.numeric(n[arm_order]),
               conversions = as.numeric(x[arm_order]),
               stringsAsFactors = FALSE)
  } else {
    if (anyDuplicated(labels)) {
      stop("With `trials`, `data` must have exactly one row per arm.",
           call. = FALSE)
    }
    x <- data[[outcome]]
    n <- data[[trials]]
    if (any(x < 0) || any(n <= 0) || any(x > n)) {
      stop("Conversions must be between 0 and `trials` for every arm.",
           call. = FALSE)
    }
    data.frame(arm = labels, n = as.numeric(n), conversions = as.numeric(x),
               stringsAsFactors = FALSE)
  }
}

compare_arm <- function(arm, n_t, x_t, n_c, x_c, z_ci, alternative) {
  p_t <- x_t / n_t
  p_c <- x_c / n_c
  diff <- p_t - p_c

  # Unpooled standard error for the confidence interval.
  se_ci <- sqrt(p_t * (1 - p_t) / n_t + p_c * (1 - p_c) / n_c)

  # Pooled standard error for the hypothesis test.
  p_pool <- (x_t + x_c) / (n_t + n_c)
  se_0 <- sqrt(p_pool * (1 - p_pool) * (1 / n_t + 1 / n_c))

  p_value <- if (se_0 == 0) {
    1
  } else {
    z <- diff / se_0
    switch(alternative,
           two.sided = 2 * stats::pnorm(-abs(z)),
           greater   = stats::pnorm(z, lower.tail = FALSE),
           less      = stats::pnorm(z))
  }

  # Relative lift CI via the log risk ratio.
  rel <- if (p_c > 0) diff / p_c else NA_real_
  if (x_t > 0 && x_c > 0) {
    se_log <- sqrt(1 / x_t - 1 / n_t + 1 / x_c - 1 / n_c)
    rel_lo <- exp(log(p_t / p_c) - z_ci * se_log) - 1
    rel_hi <- exp(log(p_t / p_c) + z_ci * se_log) - 1
  } else {
    rel_lo <- rel_hi <- NA_real_
  }

  data.frame(
    arm = arm, rate = p_t, control_rate = p_c,
    abs_lift = diff,
    abs_lift_lower = diff - z_ci * se_ci,
    abs_lift_upper = diff + z_ci * se_ci,
    rel_lift = rel, rel_lift_lower = rel_lo, rel_lift_upper = rel_hi,
    p_value = p_value,
    stringsAsFactors = FALSE
  )
}

build_summary <- function(x) {
  s <- x$settings
  conf <- sprintf("%g%%", 100 * s$conf_level)
  adjusted <- s$adjust != "none" && s$n_comparisons > 1
  p_label <- if (adjusted) "adjusted p" else "p"

  lines <- vapply(seq_len(nrow(x$results)), function(i) {
    r <- x$results[i, ]
    head <- sprintf("%s converted at %s vs %s for %s, a difference of %s (%+.1f%% relative).",
                    r$arm, fmt_pct(r$rate, 2), fmt_pct(r$control_rate, 2),
                    s$ctrl, fmt_pp(r$abs_lift), 100 * r$rel_lift)
    if (r$significant) {
      sprintf("%s This is statistically significant at the %s level%s (%s = %s).",
              head, conf,
              if (adjusted) sprintf(" after %s adjustment", s$adjust) else "",
              p_label, fmt_p(r$p_adjusted))
    } else {
      mde_txt <- if (is.na(r$achieved_mde_abs)) "" else sprintf(
        " The test could reliably detect differences of %s or more, so a smaller real effect can't be ruled out.",
        fmt_pp(abs(r$achieved_mde_abs)))
      sprintf("%s This is not statistically significant (%s = %s).%s",
              head, p_label, fmt_p(r$p_adjusted), mde_txt)
    }
  }, character(1))

  if (!is.null(x$srm) && !x$srm$passed) {
    lines <- c(sprintf(paste(
      "WARNING: sample ratio mismatch detected (p = %s). The traffic split",
      "doesn't match the intended allocation, so treat these results with",
      "caution until the cause is found."), fmt_p(x$srm$p_value)), lines)
  }

  if (!is.null(x$sizing) && !x$sizing$enough) {
    short <- names(x$sizing$actual)[x$sizing$actual < x$sizing$required]
    lines <- c(lines, sprintf(
      "The test was underpowered for the target MDE: %s fell short of the %s sample size.",
      paste(short, collapse = ", "),
      if (x$sizing$source == "plan") "planned" else "required"))
  }

  # The plan's sample size depends on the baseline it assumed.
  sz <- x$sizing
  if (!is.null(sz) && sz$source == "plan" &&
      abs(sz$observed_baseline / sz$planned_baseline - 1) > 0.2) {
    lines <- c(lines, sprintf(paste(
      "Note: the control rate (%s) differs from the planned baseline (%s)",
      "by more than 20%%, so the planned sample size may not deliver the",
      "intended power. See the achieved MDE for what the test could",
      "actually detect."),
      fmt_pct(sz$observed_baseline, 2), fmt_pct(sz$planned_baseline, 2)))
  }

  paste(lines, collapse = "\n\n")
}
