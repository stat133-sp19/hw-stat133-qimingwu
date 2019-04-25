context("Objects for binomial variables")

test_that("bin_choose returns the correct number of combinations", {
  expect_equal(bin_choose(5, 2), 10)
  expect_equal(bin_choose(7, 3), 35)
  expect_equal(bin_choose(10, 10), 1)
  expect_equal(bin_choose(0, 0), 1)
  expect_error(bin_choose(10, 20))
})

test_that("bin_probability returns the correct binomial probability", {
  expect_equal(bin_probability(1, 1, 1), 1)
  expect_equal(bin_probability(2, 10, 0.3), 0.2334744, tolerance = 1e-5)
  expect_equal(bin_probability(5, 9, 0.6), 0.2508227, tolerance = 1e-5)
  expect_error(bin_probability(5, 2, 0.2))
  expect_error(bin_probability(1.1, 6.3, 0.1))
})

test_that("bin_distribution returns the correct data frame", {
  dis1 <- bin_distribution(trials = 5, prob = 0.5)
  expect_equal(length(dis1), 2)
  expect_equal(nrow(dis1), 6)
  expect_equal(dis1$success, 0:5)
  expect_equal(dis1$probability,
               c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
})

test_that("bin_cumulative returns the correct data frame", {
  dis2 <- bin_cumulative(trials = 5, prob = 0.5)
  expect_equal(length(dis2), 3)
  expect_equal(nrow(dis2), 6)
  expect_equal(dis2$success, 0:5)
  expect_equal(dis2$probability,
               c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  expect_equal(dis2$cumulative,
               c(0.03125, 0.18750, 0.50000, 0.81250, 0.96875, 1.00000))
})
