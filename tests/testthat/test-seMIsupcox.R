test_that("seMIsup itself", {
  data(diabetic, package = "survival")

  expect_equal(
     length(seMIsupcox(Impute = TRUE, center.init.N = 20,
                       X = list(diabetic[, c(3,5:8)]),
                   Y = diabetic[, c("time", "status")],
                   nfolds = 10,
                   center.init = TRUE, return.detail = TRUE)),
     3
    )

  expect_equal(
    length(seMIsupcox(Impute = TRUE, Impute.m = 1, center.init.N = 20,
                      X = list(diabetic[, c(3,5:8)]),
                      Y = diabetic[, c("time", "status")],
                      nfolds = 10,
                      center.init = TRUE)[[1]]),
    dim(diabetic)[1]
  )

  imp <- MImpute_surv(diabetic[, c(3,5:8)], 3)
  expect_error(
    seMIsupcox(X = imp,
               Y = diabetic[, c("time", "status")],
               nfolds = 10,
               center.init = FALSE, return.detail = TRUE)
  )

  expect_equal(
    length(
      seMIsupcox(
        X = imp,
        Impute = FALSE, nfolds = 10,
        Unsup.Sup.relImp = list("E.55" = c(.5, .5),
                                "E.46" = c(.4, .6),
                                "E.64" = c(.6, .4)),
        Y = diabetic[, c("time", "status")],
        center.init.N = 20
        )
    ),
    3
  )

  ks <- sample(2:6, size = 20, replace = TRUE)
  expect_equal(
    length(seMIsupcox(X = imp, Impute = FALSE, nfolds = 10,
                      center.init = sapply(1:length(imp), function(mi.i) {
                        initiate_centers(data = imp[[mi.i]][, 1:3],
                                         N = 20, t = 1, k = ks)},
                        USE.NAMES = TRUE, simplify = FALSE),
                      Y = diabetic[, c("time", "status")],
                      center.init.N = 20)[[1]]),
    dim(diabetic)[1]
  )


  expect_equal(
    length(
      suppressWarnings(
        initiate_centers(
          data = iris[, 1:4], N = 10, t = .5,
          k = sample(2:6, size = 10, replace = TRUE),
          algorithms = sample(c("km", "hclust.mean", "hclust.med", "kmed"),
                              size = 10*.5, replace = TRUE)
        ))),
    10)

  expect_equal(
    length(
      suppressWarnings(
        initiate_centers(
          data = iris[, 1:4], N = 10, t = 0,
          k = sample(2:6, size = 10, replace = TRUE),
          algorithms = sample(c("km", "hclust.mean", "hclust.med"),
                              size = 10, replace = TRUE)
        ))),
    10)

  expect_equal(
    dim(exctract_center_position(iris[, 1], as.numeric(iris[, 5]), "colMeans")),
    c(3,1)
  )

})


test_that("Evaluation of partitions", {
  data(diabetic, package = "survival")
  part <- seMIsupcox(Impute = TRUE, center.init.N = 5,
                     X = list(diabetic[, c(3,5:8)]),
                     Y = diabetic[, c("time", "status")],
                     nfolds = 10,
                     center.init = TRUE)[[1]]
  part2 <- seMIsupcox(Impute = TRUE, center.init.N = 5,
                     X = list(diabetic[, c(3,5:8)]),
                     Y = diabetic[, c("time", "status")],
                     nfolds = 10,
                     center.init = TRUE)[[1]]
  library(survival, quietly = TRUE)

  expect_equal(length(
    evaluate_partition_semisup(part,
                               part2,
                               part,
                               part,
                               data.surv = diabetic[, c("time", "status")],
                               TMIN = 50, TMAX = 60)), 22)


})
