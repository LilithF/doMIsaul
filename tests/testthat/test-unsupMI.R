test_that("Partition sorting based on Jaccard index works correctly",
          {
            expect_equal(my_jack(iris, 0), iris)
            expect_equal(my_jack(iris[,c(1,1,1)], 0), TRUE)
            expect_equal(my_jack(iris, 0.1), FALSE)
            expect_error(my_jack(iris[,1], 0.1))

          })

test_that("UnsupMI it self", {
  expect_equal(dim(unsupMI(data = list(airquality), log.data = TRUE, Impute = "MImpute")),
               c(dim(airquality)[1], 1))

  airquality2 <- airquality
  airqualitycens <- airquality
  airquality2$Temp[airquality$Temp <= 72] <- NA
  airqualitycens$Temp[airquality$Ozone <= 72] <- 72
  airqualitycens$Temp[is.na(airqualitycens$Temp)] <- 90

  expect_equal(dim(unsupMI(data = list(airquality2), Impute = "MImpute_lcens",
                           cens.data.lod = data.frame(Temp = airqualitycens$Temp),
                           cens.standards = data.frame(Temp = 72),
                           cens.mice.log = FALSE)),
               c(dim(airquality)[1], 1))

  airquality2 <- airquality
  airqualitycens <- log(airquality)
  airqualitycens[is.na(airqualitycens)] <- 4
  airquality2$Temp[airquality$Temp <= 72] <- NA
  airqualitycens$Temp[airquality$Ozone <= 72] <- round(log(72), 10)
  airqualitycens$Temp[is.na(airqualitycens$Temp)] <- 90

  expect_equal(length(suppressWarnings(unsupMI(data = list(airquality2), Impute = "MImpute_lcens",
                           cens.data.lod = data.frame(Temp = airqualitycens$Temp),
                           cens.standards = data.frame(Temp = 72),
                           cens.mice.log = 10, return.detail = TRUE))),
               3)


  expect_equal(
    dim(unsupMI(data = list(airquality), Impute = "MImpute", algo = c("km", "hc"))),
               c(dim(airquality)[1], 2))
  expect_equal(length(unsupMI(data = list(airquality), Impute = "MImpute", return.detail = TRUE)),
               3)
  expect_equal(
    dim(unsupMI(data = list(airquality), Impute = "MImpute", not.to.use = "Ozone")),
    c(dim(airquality)[1], 1))

  expect_equal(
    dim(unsupMI(data = MImpute(airquality, 3), Impute = FALSE)),
    c(dim(airquality)[1], 1))

  expect_equal(dim(unsupMI(data = list(airquality), Impute = "MImpute", k.crit = "CritCF")),
               c(dim(airquality)[1], 1))

})

test_that("Evaluation of partitions", {

  expect_equal(length(
    evaluate_partition_unsup(unsupMI(data = list(airquality),
                                     Impute = "MImpute", Impute.m = 4),
                             factor(airquality$Month))), 10)
  expect_equal(length(
    evaluate_partition_unsup(unsupMI(data = list(airquality), Impute = "MImpute"),
                             factor(airquality$Month),
                             is.missing = !complete.cases(airquality),
                             is.cens = rep(FALSE, nrow(airquality)))), 10)


})

test_that("Plot works", {
  imp <- MImpute(airquality, 3)
  expect_s3_class(plot_MIpca(imp, 1:10, color.var = airquality$Month == 6), "ggplot")
  expect_s3_class(plot_MIpca(imp, NULL, pca.varsel = c("Ozone", "Solar.R", "Wind")), "ggplot")
  expect_s3_class(plot_MIpca(imp, "DATA$Month<6", color.var = "none", pc.sel = c(3,4)), "ggplot")
  expect_s3_class(plot_MIpca(imp, 1:10, color.var = NULL), "ggplot")

  expect_s3_class(plot_MIpca_all(imp, 1:10, color.var = airquality$Month == 6), "ggplot")
  expect_s3_class(plot_MIpca_all(imp, NULL, pca.varsel = c("Ozone", "Solar.R", "Wind")), "ggplot")
  expect_s3_class(plot_MIpca_all(imp, "DATA$Month<6", color.var = "none", pc.sel = c(3,4)), "ggplot")
  expect_s3_class(plot_MIpca_all(imp, 1:10, color.var = NULL, alpha = .6), "ggplot")

  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4]), "ggplot")
  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4],
                               vars.cont.names = paste("X", 1:4),
                               unclass.name = "virginica", include.unclass = TRUE), "ggplot")
  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4],
                               vars.cont.names = paste("X", 1:4),
                               unclass.name = "virginica", include.unclass = FALSE), "ggplot")
  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4],
                               vars.cont.names = paste("X", 1:4),
                               unclass.name = NA, include.unclass = TRUE), "ggplot")


  data(diabetic, package = "survival")
  expect_s3_class(plot_frequency(data = diabetic, partition.name = "status",
                               vars.cat = c("laser", "trt", "eye")), "ggplot")
  expect_s3_class(plot_frequency(data = diabetic, partition.name = "status",
                                 binary.simplify = FALSE,
                                 unclass.name = NA,
                                 include.unclass = TRUE,
                                 vars.cat = c("laser", "trt", "eye"),
                                 vars.cat.names = c("Laser", "Traitement", "Eye")), "ggplot")



})


test_that("multiCOns",{
  library(mclust)
  expect_equal(length(MultiCons(iris[, 1:4])), 2)
  expect_equal(length(MultiCons(iris[, 1:4], Plot = FALSE, returnAll = TRUE)), 2)
})
