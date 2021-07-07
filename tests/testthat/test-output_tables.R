test_that("Output tables", {
  expect_equal(nrow(table_continuous(iris, colnames(iris)[5],
                                colnames(iris[1:4]))), 4)
  expect_equal(nrow(
    table_continuous(iris, colnames(iris)[5],
                     colnames(iris[1:4]), na.value = NA,
                     nb.dec = 2,text.pval = TRUE,
                     vars.cont.names = paste("X", 5:8))), 4)
  expect_equal(nrow(
    table_continuous(iris, colnames(iris)[5],
                     colnames(iris[1:4]), na.value = "rien",
                     nb.dec = 2,text.pval = TRUE,
                     vars.cont.names = paste("X", 5:8))), 4)


  data(diabetic, package = "survival")
  expect_equal(
    nrow(table_categorical(diabetic, "trt",
                           c("laser", "status", "eye"))), 6)
  expect_equal(
    nrow(table_categorical(diabetic, "trt",
                           c("laser", "status", "eye"), text.pval = TRUE)), 6)
  expect_equal(
    nrow(table_categorical(diabetic, "status",
                           c("laser", "trt", "eye"), na.value = NA,
                           nb.dec = 2,text.pval = FALSE,
                           vars.cat.names = c("Laser", "Trt", "Eye") )), 6)

  expect_equal(
    nrow(table_categorical(diabetic, "status",
                           c("laser", "trt", "eye"), na.value = NA,
                           nb.dec = 2,text.pval = TRUE,
                           vars.cat.names = c("Laser", "Trt", "Eye") )), 6)

})
