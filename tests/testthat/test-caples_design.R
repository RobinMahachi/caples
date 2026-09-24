test_that("equal split matches stats::power.prop.test", {
  d <- caples_design(baseline = 0.05, mde = 0.01, power = 0.8)
  ref <- stats::power.prop.test(p1 = 0.05, p2 = 0.06, power = 0.8)$n
  expect_lte(abs(unname(d$n_per_arm[1]) - ceiling(ref)), 1)
})

test_that("relative MDE is converted correctly", {
  d <- caples_design(baseline = 0.05, mde = 0.10, mde_type = "relative")
  expect_equal(d$target_rate, 0.055)
})

test_that("smaller MDE needs more users", {
  big   <- caples_design(baseline = 0.05, mde = 0.02)
  small <- caples_design(baseline = 0.05, mde = 0.01)
  expect_gt(small$total_n, big$total_n)
})

test_that("multiple arms with adjustment need more users per arm", {
  two   <- caples_design(baseline = 0.05, mde = 0.01, arms = 2)
  three <- caples_design(baseline = 0.05, mde = 0.01, arms = 3)
  expect_gt(three$n_per_arm[1], two$n_per_arm[1])
})

test_that("unequal allocation respects the split", {
  d <- caples_design(baseline = 0.05, mde = 0.01, arms = 3,
                    allocation = c(0.5, 0.25, 0.25))
  expect_equal(unname(d$n_per_arm[2] / d$n_per_arm[1]), 0.5, tolerance = 0.001)
})

test_that("impossible MDE gives a clear error", {
  expect_error(caples_design(baseline = 0.99, mde = 0.05), "outside 0-1")
})
