#' Size an A/B/n test on a conversion rate
#'
#' Calculates the sample size needed per arm to detect a given minimum
#' detectable effect (MDE) against a control arm, for one or more treatment
#' arms, with equal or unequal traffic allocation.
#'
#' @param baseline Expected conversion rate in the control arm (between 0 and 1).
#' @param mde Minimum detectable effect.
#' @param mde_type `"absolute"` (percentage points, e.g. 0.01 = 5% -> 6%) or
#'   `"relative"` (proportional change, e.g. 0.10 = 5% -> 5.5%).
#' @param power Target power, e.g. 0.8 or 0.85.
#' @param arms Total number of arms including control.
#' @param allocation Optional traffic split, control first, e.g.
#'   `c(0.5, 0.25, 0.25)`. Defaults to an equal split. Rescaled to sum to 1.
#' @param conf_level Confidence level; significance level is `1 - conf_level`.
#' @param alternative `"two.sided"`, `"greater"` (treatment > control) or
#'   `"less"` (treatment < control).
#' @param adjust_sizing If `TRUE` and there is more than one treatment arm,
#'   alpha is divided by the number of comparisons (Bonferroni). This is the
#'   conservative choice when you plan to use a multiplicity correction such
#'   as Holm.
#'
#' @return An object of class `caples_design`: a list with the per-arm sample
#'   sizes, total sample size and the inputs used.
#' @export
#'
#' @examples
#' caples_design(baseline = 0.05, mde = 0.01)
#' caples_design(baseline = 0.05, mde = 0.10, mde_type = "relative",
#'              arms = 3, allocation = c(0.5, 0.25, 0.25))
caples_design <- function(baseline,
                         mde,
                         mde_type = c("absolute", "relative"),
                         power = 0.8,
                         arms = 2,
                         allocation = NULL,
                         conf_level = 0.95,
                         alternative = c("two.sided", "greater", "less"),
                         adjust_sizing = TRUE) {
  mde_type    <- match.arg(mde_type)
  alternative <- match.arg(alternative)

  check_prob(baseline, "baseline")
  check_prob(power, "power")
  check_prob(conf_level, "conf_level")
  if (!is.numeric(mde) || length(mde) != 1 || mde <= 0) {
    stop("`mde` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(arms) || arms < 2 || arms != round(arms)) {
    stop("`arms` must be a whole number of at least 2.", call. = FALSE)
  }

  allocation <- resolve_allocation(allocation, arms)
  n_comparisons <- arms - 1

  alpha <- 1 - conf_level
  alpha_sizing <- if (adjust_sizing && n_comparisons > 1) alpha / n_comparisons else alpha

  p_treat <- mde_to_rate(baseline, mde, mde_type, alternative)

  # Required control-arm n for each comparison, then scale to the total.
  a_ctrl <- allocation[1]
  n_ctrl_needed <- vapply(seq_len(n_comparisons), function(i) {
    k <- allocation[i + 1] / a_ctrl
    n_two_prop(p1 = baseline, p2 = p_treat, alpha = alpha_sizing,
               power = power, k = k, alternative = alternative)
  }, numeric(1))

  total <- max(n_ctrl_needed) / a_ctrl
  n_per_arm <- ceiling(total * allocation)
  names(n_per_arm) <- c("control", paste0("treatment_", seq_len(n_comparisons)))

  out <- list(
    n_per_arm     = n_per_arm,
    total_n       = sum(n_per_arm),
    baseline      = baseline,
    target_rate   = p_treat,
    mde           = mde,
    mde_type      = mde_type,
    power         = power,
    conf_level    = conf_level,
    alpha_sizing  = alpha_sizing,
    alternative   = alternative,
    allocation    = allocation,
    adjust_sizing = adjust_sizing
  )
  class(out) <- "caples_design"
  out
}


# Internal: sample size for the control arm of a two-proportion z-test.
# k = n_treatment / n_control. Matches stats::power.prop.test() when k = 1.
n_two_prop <- function(p1, p2, alpha, power, k = 1, alternative = "two.sided") {
  sides <- if (alternative == "two.sided") 2 else 1
  z_a <- stats::qnorm(1 - alpha / sides)
  z_b <- stats::qnorm(power)
  p_bar <- (p1 + k * p2) / (1 + k)

  num <- (z_a * sqrt(p_bar * (1 - p_bar) * (1 + 1 / k)) +
            z_b * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k))^2
  num / (p2 - p1)^2
}


# Internal: power of a two-proportion z-test for given group sizes.
power_two_prop <- function(p1, p2, n1, n2, alpha, alternative = "two.sided") {
  sides <- if (alternative == "two.sided") 2 else 1
  z_a <- stats::qnorm(1 - alpha / sides)
  p_bar <- (n1 * p1 + n2 * p2) / (n1 + n2)
  se_null <- sqrt(p_bar * (1 - p_bar) * (1 / n1 + 1 / n2))
  se_alt  <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  stats::pnorm((abs(p2 - p1) - z_a * se_null) / se_alt)
}


# Internal: smallest absolute lift detectable at the target power, given the
# sample sizes actually achieved.
achieved_mde <- function(p1, n1, n2, alpha, power, alternative = "two.sided") {
  direction <- if (alternative == "less") -1 else 1
  upper <- if (direction == 1) 1 - p1 - 1e-9 else p1 - 1e-9
  if (upper <= 1e-6) return(NA_real_)

  f <- function(d) {
    power_two_prop(p1, p1 + direction * d, n1, n2, alpha, alternative) - power
  }
  if (f(upper) < 0) return(NA_real_)  # not detectable at any effect size
  stats::uniroot(f, lower = 1e-9, upper = upper, tol = 1e-10)$root
}
