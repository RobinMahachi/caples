
# caples

Fast, plain-English readouts for A/B/n tests on conversion rates.

Named after John Caples, who pioneered tested advertising.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("RobinMahachi/caples")
```

## Workflow

Plan first, analyse second.

``` r
library(caples)

# 1. Plan the test before it runs
plan <- caples_design(baseline = 0.05, mde = 0.01, power = 0.85, arms = 3)
plan
#> caples test design
#> -----------------
#> Baseline rate:   5.00%
#> Target rate:     6.00%  (MDE 0.01 absolute)
#> Power:           85%
#> Alpha (sizing):  0.0250  (two.sided)
#> 
#> Required sample size per arm:
#>          arm share      n
#>      control 33.3% 11,167
#>  treatment_1 33.3% 11,167
#>  treatment_2 33.3% 11,167
#> 
#> Total: 33,501

# 2. Run the test (simulated here)
set.seed(42)
n <- plan$n_per_arm
df <- data.frame(
  arm = rep(c("control", "A", "B"), times = n),
  converted = c(rbinom(n[1], 1, 0.050),
                rbinom(n[2], 1, 0.052),
                rbinom(n[3], 1, 0.062))
)

# 3. Read it out against the plan
caples_test(df, group = "arm", outcome = "converted",
            ctrl = "control", design = plan)
#> caples test readout
#> ------------------
#> Control: control | Confidence: 95% | Adjustment: holm | two.sided
#> 
#> Control rate: 4.90% (n = 11,167)
#> 
#>  arm  rate abs_lift               abs_ci rel_lift       p   p_adj sig
#>    A 5.27% +0.38 pp [-0.20 pp, +0.95 pp]    +7.7%   0.201   0.201  no
#>    B 6.70% +1.80 pp [+1.19 pp, +2.41 pp]   +36.7% < 0.001 < 0.001 yes
#> 
#> SRM check: passed (p = 1.000)
#> Sample size vs plan: sufficient
#> 
#> Summary
#> -------
#> A converted at 5.27% vs 4.90% for control, a difference of +0.38 pp (+7.7%
#> relative). This is not statistically significant (adjusted p = 0.201). The test
#> could reliably detect differences of +0.99 pp or more, so a smaller real effect
#> can't be ruled out.
#> 
#> B converted at 6.70% vs 4.90% for control, a difference of +1.80 pp (+36.7%
#> relative). This is statistically significant at the 95% level after holm
#> adjustment (adjusted p = < 0.001).
```

## What it does

- Sizes tests with equal or unequal traffic splits and multiple arms
- Pooled two-proportion z-tests with multiplicity correction (Holm by
  default)
- Absolute and relative lifts with confidence intervals
- Sample ratio mismatch (SRM) check
- Sample size vs plan, and the achieved minimum detectable effect
- A plain-English summary for stakeholders
