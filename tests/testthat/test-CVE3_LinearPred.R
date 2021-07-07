test_that("CVE need survival data", {
  expect_error(cve_LinearPred(iris[, 1:4], iris[,4], 10))
})
