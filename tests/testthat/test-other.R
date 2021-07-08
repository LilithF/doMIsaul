test_that("pformat", {
  expect_equal(formatpv(0.0005),  "0.0005")
  expect_equal(formatpv(0.001 ),  "0.001")
})

test_that("mi impute", {
  pm <- mice::mice(airquality, maxit = 0)$predictorMatrix
  expect_equal(length(MImpute(airquality, 3,
                              method = c("pmm", "pmm", "", "", "", ""),
                              predMat = pm,
                              return.midsObject = TRUE)), 2)
  expect_equal(length(MImpute(airquality[, -1], 2)), 2)

})


test_that("mi impute for survival", {
  data(cancer, package = "survival")
  expect_equal(length(MImpute_surv(cancer, 3,
                              return.midsObject = TRUE)), 2)

})

test_that("mi impute for censored", {
  airquality2 <- airquality
  airqualitycens <- log(airquality)
  airqualitycens[is.na(airqualitycens)] <- 4
  airquality2$Temp[airquality$Temp <= 72] <- NA
  airqualitycens$Temp[airquality$Ozone <= 72] <- round(log(72), 10)
  airqualitycens$Temp[is.na(airqualitycens$Temp)] <- 90

  expect_equal(length(
    MImpute_lcens(data = airquality2, mi.m = 2,
                  data.lod = data.frame(Temp = airqualitycens$Temp),
                  standards = data.frame(Temp = 72),
                  mice.log = 10, return.midsObject = TRUE)),
               2)
})

test_that("partition generation", {
  suppressWarnings(library(mclust, quietly = TRUE))
  expect_equal(dim(
    partition_generation(iris[, 1:4], LOG = FALSE,
                         clust.algo = c("kmed", "mclust", "hc"),
                         k.crit = "ch")),
    c(dim(iris)[1], 3))

  expect_equal(dim(
    partition_generation(iris[, 1:4], LOG = FALSE,
                         clust.algo = c("km", "mclust", "hc"),
                         k.crit = "CritCF")),
    c(dim(iris)[1], 3))
  expect_equal(dim(
    partition_generation(iris[, 1:4], LOG = FALSE,
                         clust.algo = c("mclust"),
                         k.crit = "bic")),
    c(dim(iris)[1], 1))


})


test_that("MIclust mpool", {
  expect_equal(
    dim(MIclust_mpool(list(data.frame(A = factor(rep(c(1,2), times = 50))),
                           data.frame(A = factor(rep(c(1,2), times = 50)))),
                      comb.cons = FALSE)),
    c(100, 1))

  expect_equal(
    dim(MIclust_mpool(list(data.frame(A = factor(rep(c(1,2), times = 50))),
                           data.frame(A = factor(rep(c(1,2), each = 50)))),
                      comb.cons = FALSE, mcons.JAC.sel = .4)),
    c(100, 1))

  expect_equal(
    dim(
      my_jack(data.frame(A = factor(rep(c(1,2), each = 50)),
                         B = factor(rep(c(1,2), times = c(52, 48))),
                         B2 = factor(rep(c(1,2), times = c(45, 55))),
                         C = factor(rep(c(1,2), times = 50))),
                mcons.JAC.sel = .6)
        ),
    c(100, 3))

})


test_that("Clean up partitions", {

  expect_equal(
    length(cleanUp_partition(
      factor(rep(c(1,2, 3), times = c(50, 50, 5))),
      level.order = runif(105))),
    105)


})


