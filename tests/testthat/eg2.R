source(here::here("tests","testthat","eg_code2.R"))


testthat::test_that("a", {
  testthat::expect_equal(digest::digest(a), "a63c70e73b58d0823ab3bcbd3b543d6f")
})

testthat::test_that("b", {
  testthat::expect_equal(digest::digest(b), "3e8cd2c875965da106a328686556c864")
})


