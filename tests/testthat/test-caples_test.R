make_data <- function() {
  data.frame(
    arm = rep(c("control", "A", "B"), times = c(10000, 10000, 10000)),
    converted = c(rep(1:0, c(500, 9500)),
                  rep(1:0, c(520, 9480)),
                  rep(1:0, c(620, 9380)))
  )
}

test_that("p-values match prop.test without continuity correction", {
  res <- caples_test(make_data(), "arm", "converted", ctrl = "control",
                    adjust = "none")
  ref <- stats::prop.test(c(620, 500), c(10000, 10000), correct = FALSE)$p.value
  expect_equal(res$results$p_value[res$results$arm == "B"], ref)
})

test_that("row-level and aggregated input give identical results", {
  agg <- data.frame(arm = c("control", "A", "B"),
                    conv = c(500, 520, 620), n = c(10000, 10000, 10000))
  r1 <- caples_test(make_data(), "arm", "converted", ctrl = "control")
  r2 <- caples_test(agg, "arm", "conv", ctrl = "control", trials = "n")
  expect_equal(r1$results, r2$results)
})

test_that("holm adjustment is applied", {
  res <- caples_test(make_data(), "arm", "converted", ctrl = "control",
                    adjust = "holm")
  expect_equal(res$results$p_adjusted,
               stats::p.adjust(res$results$p_value, "holm"))
})

test_that("SRM is flagged on a badly skewed split", {
  agg <- data.frame(arm = c("control", "A"), conv = c(500, 550),
                    n = c(10000, 11000))
  res <- caples_test(agg, "arm", "conv", ctrl = "control", trials = "n",
                    allocation = c(0.5, 0.5))
  expect_false(res$srm$passed)
})

test_that("underpowered tests are flagged", {
  agg <- data.frame(arm = c("control", "A"), conv = c(5, 6), n = c(100, 100))
  res <- caples_test(agg, "arm", "conv", ctrl = "control", trials = "n",
                    mde = 0.01)
  expect_false(res$sizing$enough)
})

test_that("helpful errors for bad input", {
  expect_error(caples_test(make_data(), "arm", "converted", ctrl = "ctrl"),
               "not found")
  expect_error(caples_test(make_data(), "arm", "oops", ctrl = "control"),
               "not found")
  expect_error(caples_test(make_data(), "arm", "converted", ctrl = "control",
                          adjust = "banana"), "must be one of")
})

test_that("print method runs", {
  res <- caples_test(make_data(), "arm", "converted", ctrl = "control",
                    mde = 0.01, allocation = c(1, 1, 1))
  expect_output(print(res), "caples test readout")
})

test_that("design argument uses the plan's settings and planned n", {
  plan <- caples_design(baseline = 0.05, mde = 0.01, power = 0.85, arms = 3)
  res <- caples_test(make_data(), "arm", "converted", ctrl = "control",
                    design = plan)
  expect_equal(res$sizing$source, "plan")
  expect_equal(unname(res$sizing$required), unname(plan$n_per_arm))
  expect_equal(res$settings$conf_level, plan$conf_level)
  expect_false(is.null(res$srm))  # allocation taken from the plan
})

test_that("passing values alongside design warns", {
  plan <- caples_design(baseline = 0.05, mde = 0.01, arms = 3)
  expect_warning(
    caples_test(make_data(), "arm", "converted", ctrl = "control",
               design = plan, power = 0.9),
    "from `design`"
  )
})

test_that("design with the wrong number of arms errors", {
  plan <- caples_design(baseline = 0.05, mde = 0.01, arms = 2)
  expect_error(
    caples_test(make_data(), "arm", "converted", ctrl = "control",
               design = plan),
    "planned for 2 arms"
  )
})

test_that("design must come from caples_design()", {
  expect_error(
    caples_test(make_data(), "arm", "converted", ctrl = "control",
               design = list(mde = 0.01)),
    "must be the output"
  )
})
