test_that("Output tables", {
  expect_equal(nrow(table_continuous(iris, colnames(iris)[5],
                                colnames(iris[1:4]))), 4)
})
