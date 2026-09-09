test_that("p_rope", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_or_load_if_installed("rstanarm")
  m <- insight::download_model("stanreg_merMod_5")
  expect_equal(
    p_rope(as.data.frame(m)[2:4], range = list(c(0, 40), "default", c(-1, 0.8)))$p_ROPE,
    c(0.598, 0.002, 0.396),
    tolerance = 1e-3
  )

  expect_error(
    p_rope(as.data.frame(m)[2:4], range = list(c(0, 40), c(-1, 0.8))),
    regex = "Length of"
  )
  expect_error(
    p_rope(as.data.frame(m)[2:4], range = list(c(0, 40), "a", c(-1, 0.8))),
    regex = "should be 'default'"
  )
})
