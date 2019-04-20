context("Check binomial arguments")

test_that("check_prob works with valid probabilities", {
  expect_true(check_prob(0))
  expect_true(check_prob(1))
  expect_true(check_prob(0.5))
})

test_that("check_prob stops with invalid probabilities", {
  expect_error(check_prob(-0.1))
  expect_error(check_prob(1.2))
  expect_error(check_prob(-1))
})

test_that("check_trials works with valid number of trials", {
  expect_true(check_trials(0))
  expect_true(check_trials(5))
  expect_true(check_trials(10))
})

test_that("check_trials stops with invalid number of trials", {
  expect_error(check_trials(-1))
  expect_error(check_trials(2.5))
  expect_error(check_trials(-1.6))
})

test_that("check_success works with valid number of successes", {
  expect_true(check_success(0, 0))
  expect_true(check_success(5, 10))
  expect_true(check_success(c(2, 3, 4), 4))
})

test_that("check_success stops with invalid number of successes", {
  expect_error(check_success(2, 1))
  expect_error(check_success(c(2, 3, 4), 3))
  expect_error(check_success(c(2.1, 3, 4), 5))
})
