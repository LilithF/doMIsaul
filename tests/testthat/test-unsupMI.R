context("Unsupervised learning")

test_that("machin works fine", {
  expect_equal(unsupMI(XX), 4)
  expect_equal(2 * 2, 4)
  expect_equal(2 * 2, 4)
})


test_that("A warning appears", {
  expect_waring(unsupMI(SS))
})


test_that("Correct class", {
  expect_is(unsupMI(XX), "factor")
})
