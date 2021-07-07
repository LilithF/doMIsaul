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

  imp <- MImpute_surv(diabetic[, c(3,5:8)], 3)
  expect_equal(
    length(seMIsupcox(X = imp,
                   Impute = FALSE, nfolds = 10,
                   Y = diabetic[, c("time", "status")],
                   center.init.N = 20)[[1]]),
    dim(diabetic)[1]
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
        initiate_centers(data = iris[, 1:4], N = 10, t = .5,
                       k = sample(2:6, size = 10, replace = TRUE),
                       algorithms = c("km", "hclust.mean", "hclust.med", "kmed")))),
    10)

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

  expect_equal(length(
    evaluate_partition_semisup(part,
                               part,
                               part,
                               part,
                               data.surv = diabetic[, c("time", "status")],
                               TMIN = 50, TMAX = 60)), 22)

  # expect_equal(length(
  #   evaluate_partition_semisup(part,
  #                              diabetic$laser,
  #                              diabetic$eye,
  #                              diabetic$trt,
  #                              data.surv = diabetic[, c("time", "status")],
  #                              TMIN= 1, TMAX = 4)), 22)


})
