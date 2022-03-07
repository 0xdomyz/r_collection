library(testthat)

expect_aprox_equal <- function(a, b, decimal=5) {
  expect_equal(round(a, decimal), round(b, decimal))
}

test_that("part1", {
  expect_aprox_equal(0.0125, 0.0125001)
  expect_aprox_equal(0.0125, 0.012501)
})

test_that("part1", {
  expect_aprox_equal(0.0125, 0.0125001)
  expect_aprox_equal(0.0125, 0.012501)
})

test_that("part1", {
  expect_aprox_equal(0.0125, 0.0125001)
  expect_aprox_equal(0.0125, 0.012501)
})

