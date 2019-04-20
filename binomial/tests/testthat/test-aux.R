context("Auxilary functions for binomial measures")

test_that("aux_mean returns the correct mean", {
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(11, 0.5), 5.5)
  expect_equal(aux_mean(20, 0.05), 1)
})

test_that("aux_variance returns the correct variance", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(11, 0.5), 2.75)
  expect_equal(aux_variance(20, 0.05), 0.95)
})

test_that("aux_mode returns the correct mode", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(11, 0.5), c(6, 5))
  expect_equal(aux_mode(20, 0.05), 1)
})

test_that("aux_skewness returns the correct skewness", {
  expect_equal(aux_skewness(10, 0.3), 0.2760262, tolerance = 1e-5)
  expect_equal(aux_skewness(11, 0.5), 0)
  expect_equal(aux_skewness(20, 0.05), 0.9233805, tolerance = 1e-5)
})

test_that("aux_kurtosis returns the correct kurtosis", {
  expect_equal(aux_kurtosis(10, 0.3), -0.1238095, tolerance = 1e-5)
  expect_equal(aux_kurtosis(11, 0.5), -0.1818182, tolerance = 1e-5)
  expect_equal(aux_kurtosis(20, 0.05), 0.7526316, tolerance = 1e-5)
})
