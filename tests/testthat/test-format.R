test_that("p_significance", {
  set.seed(333)
  x <- rnorm(100)
  expect_equal(
    format(point_estimate(x)),
    data.frame(Median = "0.05", Mean = "-0.02", MAP = "0.13", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(ci(x)),
    data.frame(`95% CI` = "[-1.93, 1.77]", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(p_rope(x)),
    data.frame(ROPE = "[-0.10, 0.10]", `p (ROPE)` = "0.100", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(bayesfactor_parameters(x, verbose = FALSE)),
    data.frame(BF = "1.00", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(map_estimate(x)),
    data.frame(x = "0.13", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(p_direction(x)),
    data.frame(x = "0.51", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(p_map(x)),
    data.frame(x = "0.97", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  expect_equal(
    format(p_significance(x)),
    data.frame(x = "0.46", stringsAsFactors = FALSE),
    ignore_attr = TRUE
  )
  # format(rope(x))
  # format(equivalence_test(x))
})
