test_that("Partition sorting based on Jaccard index works correctly",
          {
            expect_equal(my_jack(iris, 0), iris)
            expect_equal(my_jack(iris[,c(1,1,1)], 0), TRUE)
            expect_equal(my_jack(iris, 0.7), FALSE)
            expect_error(my_jack(iris[,1], 0.1))

          })

test_that("UnsupMI it self", {
  expect_equal(
    dim(
      unsupMI(data = list(airquality), log.data = TRUE, Impute = "MImpute")
    ),
    c(dim(airquality)[1], 1))

  expect_equal(
    dim(
      unsupMI(data = list(airquality), Impute = "MImpute", Impute.m = 1)
    ),
    c(dim(airquality)[1], 1))

  airquality2 <- airquality
  airqualitycens <- airquality
  airquality2$Temp[airquality$Temp <= 72] <- NA
  airqualitycens$Temp[airquality$Ozone <= 72] <- 72
  airqualitycens$Temp[is.na(airqualitycens$Temp)] <- 90

  expect_equal(
    dim(
      unsupMI(data = list(airquality2), Impute = "MImpute_lcens",
              cens.data.lod = data.frame(Temp = airqualitycens$Temp),
              cens.standards = data.frame(Temp = 72),
              cens.mice.log = FALSE)
    ),
    c(dim(airquality)[1], 1))

  airquality2 <- airquality
  airqualitycens <- log(airquality)
  airqualitycens[is.na(airqualitycens)] <- 4
  airquality2$Temp[airquality$Temp <= 72] <- NA
  airqualitycens$Temp[airquality$Ozone <= 72] <- round(log(72), 10)
  airqualitycens$Temp[is.na(airqualitycens$Temp)] <- 90

  expect_equal(
    length(
      suppressWarnings(
        unsupMI(data = list(airquality2), Impute = "MImpute_lcens",
                cens.data.lod = data.frame(Temp = airqualitycens$Temp),
                cens.standards = data.frame(Temp = 72),
                cens.mice.log = 10, return.detail = TRUE)
      )),
    3)

  data(diabetic, package = "survival")
  expect_equal(
    dim(
      unsupMI(data = list(diabetic[, c(3,5:8)]), Impute = "MImpute_surv")
    ),
    c(dim(diabetic)[1], 1)
  )

  expect_equal(
    dim(
      unsupMI(data = list(airquality),
              Impute = "MImpute",
              algo = c("km", "hc"),
              comb.cons = TRUE)
    ),
    c(dim(airquality)[1], 3))

  expect_equal(
    length(
      unsupMI(data = list(airquality),
              Impute = "MImpute",
              algo = c("km", "hc"),
              return.detail = TRUE)
    ),
    3)

  expect_equal(
    length(
      unsupMI(data = list(airquality),
              Impute = "MImpute",
              return.detail = TRUE)
    ),
    3)

  expect_equal(
    dim(
      unsupMI(data = list(airquality),
              Impute = "MImpute",
              not.to.use = "Ozone")
    ),
    c(dim(airquality)[1], 1))

  expect_equal(
    dim(
      unsupMI(data = MImpute(airquality, 3), Impute = FALSE)
    ),
    c(dim(airquality)[1], 1))

  expect_equal(
    dim(
      unsupMI(data = list(airquality), Impute = "MImpute", k.crit = "CritCF")
    ),
    c(dim(airquality)[1], 1))

})

test_that("Evaluation of partitions", {

  expect_equal(length(
    evaluate_partition_unsup(unsupMI(data = list(airquality),
                                     Impute = "MImpute", Impute.m = 4),
                             factor(airquality$Month))), 10)
  expect_equal(length(
    evaluate_partition_unsup(unsupMI(data = list(airquality),
                                     Impute = "MImpute"),
                             factor(airquality$Month),
                             is.missing = !complete.cases(airquality),
                             is.cens = rep(FALSE, nrow(airquality)))), 10)
  expect_equal(length(
    evaluate_partition_unsup(rep(NA, nrow(airquality)),
                             factor(airquality$Month))), 10)



})

test_that("Plot works", {
  imp <- MImpute(airquality, 3)
  expect_error(plot_MIpca(imp, NULL, pc.sel = 1))
  expect_error(plot_MIpca(imp, NULL, pc.sel = c("A", "B")))
  expect_error(plot_MIpca(imp, NULL, pc.sel = c(1, 7)))

  expect_s3_class(plot_MIpca(imp, 1:10, color.var = airquality$Month == 6),
                  "ggplot")
  expect_s3_class(plot_MIpca(imp,
                             NULL,
                             pca.varsel = c("Ozone", "Solar.R", "Wind")),
                  "ggplot")
  expect_s3_class(plot_MIpca(imp,
                             "DATA$Month<6",
                             color.var = "none",
                             pc.sel = c(3,4)),
                  "ggplot")
  expect_s3_class(plot_MIpca(imp, 1:10, color.var = NULL),
                  "ggplot")

  imp2 <- lapply(MImpute(airquality, 3), function(x){rownames(x) <- NULL ; x})
  expect_s3_class(plot_MIpca(imp2, 1:10),
                  "ggplot")


  expect_error(plot_MIpca_all(imp, NULL, pc.sel = 1))
  expect_error(plot_MIpca_all(imp, NULL, pc.sel = c("A", "B")))
  expect_error(plot_MIpca_all(imp, NULL, pc.sel = c(1, 7)))


  expect_s3_class(plot_MIpca_all(imp, 1:10, color.var = airquality$Month == 6),
                  "ggplot")
  expect_s3_class(plot_MIpca_all(imp,
                                 NULL,
                                 pca.varsel = c("Ozone", "Solar.R", "Wind")),
                  "ggplot")
  expect_s3_class(plot_MIpca_all(imp,
                                 "DATA$Month<6",
                                 color.var = "none",
                                 pc.sel = c(3,4)),
                  "ggplot")
  expect_s3_class(plot_MIpca_all(imp, 1:10, color.var = NULL, alpha = .6),
                  "ggplot")
  expect_s3_class(plot_MIpca_all(imp2, 1:10), "ggplot")



  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4]),
                  "ggplot")
  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4],
                               vars.cont.names = paste("X", 1:4),
                               unclass.name = "virginica",
                               include.unclass = TRUE, add.n = TRUE),
                  "ggplot")
  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4],
                               vars.cont.names = paste("X", 1:4),
                               unclass.name = "virginica",
                               include.unclass = FALSE),
                  "ggplot")
  expect_s3_class(plot_boxplot(data = iris, partition.name = "Species",
                               vars.cont = colnames(iris)[1:4],
                               vars.cont.names = paste("X", 1:4),
                               unclass.name = NA, include.unclass = TRUE,
                               add.n = TRUE),
                  "ggplot")


  data(diabetic, package = "survival")
  expect_s3_class(plot_frequency(data = diabetic, partition.name = "status",
                                 vars.cat = c("laser", "trt", "eye")), "ggplot")

  expect_s3_class(plot_frequency(data = diabetic, partition.name = "status",
                                 binary.simplify = FALSE,
                                 unclass.name = NA,
                                 include.unclass = TRUE,
                                 vars.cat = c("laser", "trt", "eye"),
                                 vars.cat.names = c("Laser", "Trait.", "Eye")),
                  "ggplot")



})


test_that("multiCOns",{
  if (requireNamespace("mclust")){
    suppressWarnings(library(mclust, quietly = TRUE, verbose = FALSE))
    expect_equal(
      length(
        MultiCons(iris[, 1:4],
                  Clustering_selection = c("kmeans", "pam", "OPTICS")
        )), 2)

    expect_equal(
      length(
        MultiCons(iris[, 1:4],
                  Clustering_selection = c("agghc",  "AGNES", "DIANA")
        )), 2)

    expect_equal(
      length(
        MultiCons(iris[, 1:4],
                  Clustering_selection = c("MCLUST", "CMeans",
                                           "FANNY", "BaggedClust"),
                  num_algo = 13
        )), 2)

    expect_equal(
      length(
        MultiCons(iris[, 1:4], Plot = FALSE, returnAll = TRUE
        )), 2)
  }
})
