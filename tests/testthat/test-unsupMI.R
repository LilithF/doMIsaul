test_that("Partition sorting based on Jaccard index works correctly",
          {
            expect_equal(my_jack(iris, 0), iris)
            expect_equal(my_jack(iris[,c(1,1,1)], 0), TRUE)
            expect_equal(my_jack(iris, 0.1), FALSE)
            expect_error(my_jack(iris[,1], 0.1))

          })
