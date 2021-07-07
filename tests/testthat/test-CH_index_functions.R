test_that("CH function work", {
  expect_equal(CH.sel(iris[, 1:4], 2, 5, "kmed")$Best.nc, 3)
  expect_error(CH.sel(iris[, 1:4], 2, 5, "km"))
})
