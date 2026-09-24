# Internal helpers — not exported.

check_prob <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x >= 1) {
    stop(sprintf("`%s` must be a single number between 0 and 1.", name),
         call. = FALSE)
  }
}

resolve_allocation <- function(allocation, arms, labels = NULL) {
  if (is.null(allocation)) return(rep(1 / arms, arms))

  if (!is.numeric(allocation) || any(allocation <= 0)) {
    stop("`allocation` must be positive numbers.", call. = FALSE)
  }
  if (length(allocation) != arms) {
    stop(sprintf("`allocation` has %d values but there are %d arms.",
                 length(allocation), arms), call. = FALSE)
  }
  # If named and labels supplied, reorder to match (control first).
  if (!is.null(names(allocation)) && !is.null(labels)) {
    missing <- setdiff(labels, names(allocation))
    if (length(missing) > 0) {
      stop("`allocation` names don't match arms: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    allocation <- allocation[labels]
  }
  allocation / sum(allocation)
}

mde_to_rate <- function(baseline, mde, mde_type, alternative) {
  direction <- if (alternative == "less") -1 else 1
  p <- if (mde_type == "absolute") {
    baseline + direction * mde
  } else {
    baseline * (1 + direction * mde)
  }
  if (p <= 0 || p >= 1) {
    stop("The MDE pushes the target rate outside 0-1. Check `mde` and ",
         "`mde_type` (0.01 absolute = 1 percentage point; 0.01 relative = 1%).",
         call. = FALSE)
  }
  p
}

fmt_pct <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

fmt_pp <- function(x, digits = 2) {
  sprintf(paste0("%+.", digits, "f pp"), 100 * x)
}

fmt_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
}
