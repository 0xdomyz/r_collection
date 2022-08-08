testthat::test_that("aaa", {
  testthat::expect_equal(1, 1)
})


testthat::test_that("aaa", {
  # print(paste0("hash is: ", digest::digest(mtcars)))
  testthat::expect_equal(digest::digest(mtcars), "a63c70e73b58d0823ab3bcbd3b543d6f")
})

