#' @export
print.caples_design <- function(x, ...) {
  cat("caples test design\n")
  cat("-----------------\n")
  cat(sprintf("Baseline rate:   %s\n", fmt_pct(x$baseline, 2)))
  cat(sprintf("Target rate:     %s  (MDE %s %s)\n", fmt_pct(x$target_rate, 2),
              format(x$mde), x$mde_type))
  cat(sprintf("Power:           %g%%\n", 100 * x$power))
  cat(sprintf("Alpha (sizing):  %.4f  (%s)\n", x$alpha_sizing, x$alternative))
  cat("\nRequired sample size per arm:\n")
  df <- data.frame(arm = names(x$n_per_arm),
                   share = fmt_pct(x$allocation, 1),
                   n = format(x$n_per_arm, big.mark = ","))
  print(df, row.names = FALSE)
  cat(sprintf("\nTotal: %s\n", format(x$total_n, big.mark = ",")))
  invisible(x)
}

#' @export
print.caples_test <- function(x, ...) {
  s <- x$settings
  cat("caples test readout\n")
  cat("------------------\n")
  cat(sprintf("Control: %s | Confidence: %g%% | Adjustment: %s | %s\n\n",
              s$ctrl, 100 * s$conf_level, s$adjust, s$alternative))

  r <- x$results
  tbl <- data.frame(
    arm       = r$arm,
    rate      = fmt_pct(r$rate, 2),
    abs_lift  = fmt_pp(r$abs_lift),
    abs_ci    = sprintf("[%s, %s]", fmt_pp(r$abs_lift_lower), fmt_pp(r$abs_lift_upper)),
    rel_lift  = sprintf("%+.1f%%", 100 * r$rel_lift),
    p         = fmt_p(r$p_value),
    p_adj     = fmt_p(r$p_adjusted),
    sig       = ifelse(r$significant, "yes", "no")
  )
  cat(sprintf("Control rate: %s (n = %s)\n\n", fmt_pct(x$arms$rate[1], 2),
              format(x$arms$n[1], big.mark = ",")))
  print(tbl, row.names = FALSE)

  if (!is.null(x$srm)) {
    cat(sprintf("\nSRM check: %s (p = %s)\n",
                if (x$srm$passed) "passed" else "FAILED", fmt_p(x$srm$p_value)))
  }
  if (!is.null(x$sizing)) {
    label <- if (x$sizing$source == "plan") "Sample size vs plan" else "Sample size for target MDE"
    cat(sprintf("%s: %s\n", label,
                if (x$sizing$enough) "sufficient" else "INSUFFICIENT"))
  }

  cat("\nSummary\n-------\n")
  cat(strwrap(x$summary, width = 80, prefix = "", initial = ""), sep = "\n")
  invisible(x)
}
