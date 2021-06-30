#' Modified \code{ncvreg:::loss.ncvsurv}.
#'
#' modification of the \code{loss.ncvsurv()} function (\code{ncvreg} internal)
#' to handle a single value of lambda
#'
#' @param y See \code{ncvreg:::loss.ncvsurv}.
#' @param eta See \code{ncvreg:::loss.ncvsurv}.
#' @param total See \code{ncvreg:::loss.ncvsurv}.
#'
#' @return See \code{ncvreg:::loss.ncvsurv}.
mylossncvsurv <- function (y, eta, total = TRUE){

  ind <- order(y[, 1])
  d <- as.double(y[ind, 2])
  if (is.matrix(eta)) {
    eta <- eta[ind, , drop = FALSE]
    r <- apply(eta, 2, function(x) rev(cumsum(rev(exp(x)))))
  } else {
    eta <- matrix(eta[ind], ncol = 1)
    r <- matrix(rev(cumsum(rev(exp(eta)))), ncol = 1)
  }
  if (total) {
    return(-2 * (crossprod(d, eta) - crossprod(d, log(r))))
  } else {
    return(-2 * (eta[d == 1, ] - log(r)[d == 1, ]))
  }
}


#' Modified \code{ncvreg:::cv.ncvsurv}.
#'
#' Modification of the \code{cv.ncvsurv()} function of \code{ncvreg} to use the
#' modified functions \code{ncvsurv()} : \code{my.ncvsurv()} and
#' \code{loss.ncvsurv()} : \code{mylossncvsurv()}.
#'
#' @param X See \code{ncvreg:::cv.ncvsurv}.
#' @param y See \code{ncvreg:::cv.ncvsurv}.
#' @param ... See \code{ncvreg:::cv.ncvsurv}.
#' @param cluster See \code{ncvreg:::cv.ncvsurv}.
#' @param nfolds See \code{ncvreg:::cv.ncvsurv}.
#' @param seed See \code{ncvreg:::cv.ncvsurv}.
#' @param fold See \code{ncvreg:::cv.ncvsurv}.
#' @param se See \code{ncvreg:::cv.ncvsurv}.
#' @param returnY See \code{ncvreg:::cv.ncvsurv}.
#' @param trace See \code{ncvreg:::cv.ncvsurv}.
#'
#' @import ncvreg

my.cv.ncvsurv <- function (X, y, ..., cluster, nfolds = 10, seed, fold,
                           se = c("quick", "bootstrap"), returnY = FALSE,
                           trace = FALSE){
  se <- match.arg(se)
  fit.args <- list(...)
  fit.args$X <- X
  fit.args$y <- y
  fit.args$returnX <- TRUE
  fit <- do.call("my.ncvsurv", fit.args)
  X <- fit$X
  y <- cbind(fit$time, fit$fail)
  returnX <- list(...)$returnX
  if (is.null(returnX) || !returnX)
    fit$X <- NULL
  n <- nrow(X)
  sde <- sqrt(.Machine$double.eps)
  if (!missing(seed))
    set.seed(seed)
  if (missing(fold)) {
    ind1 <- which(fit$fail == 1)
    ind0 <- which(fit$fail == 0)
    n1 <- length(ind1)
    n0 <- length(ind0)
    fold1 <- 1:n1%%nfolds
    fold0 <- (n1 + 1:n0)%%nfolds
    fold1[fold1 == 0] <- nfolds
    fold0[fold0 == 0] <- nfolds
    fold <- integer(n)
    fold[fit$fail == 1] <- sample(fold1)
    fold[fit$fail == 0] <- sample(fold0)
  }
  else {
    fold <- fold[fit$order]
    nfolds <- max(fold)
  }
  Y <- matrix(NA, nrow = n, ncol = length(fit$lambda))
  cv.args <- list(...)
  cv.args$lambda <- fit$lambda
  cv.args$warn <- FALSE
  cv.args$convex <- FALSE
  cv.args$penalty.factor <- fit$penalty.factor
  if (!missing(cluster)) {
    if (!inherits(cluster, "cluster"))
      stop("cluster is not of class 'cluster'; see ?makeCluster",
           call. = FALSE)
    parallel::clusterExport(cluster, c("fold", "fit", "X",
                                       "y", "cv.args"), envir = environment())
    parallel::clusterCall(cluster, function() requireNamespace(ncvreg))
    fold.results <- parallel::parLapply(cl = cluster, X = 1:nfolds,
                                        fun = ncvreg:::cvf.surv, XX = X, y = y,
                                        fold = fold, cv.args = cv.args)
  }
  for (i in 1:nfolds) {
    if (!missing(cluster)) {
      res <- fold.results[[i]]
    }
    else {
      if (trace)
        cat("Starting CV fold #", i, sep = "", "\n")
      res <- ncvreg:::cvf.surv(i, X, y, fold, cv.args)
    }
    Y[fold == i, 1:res$nl] <- res$yhat
  }

  ind <- which(apply(is.finite(Y), 2, all))
  Y <- Y[, ind]
  lambda <- fit$lambda[ind]

  if (se == "quick") {
    L <- matrix(mylossncvsurv(y, Y, total = FALSE))
    cve <- apply(L, 2, sum)/sum(fit$fail)
    cvse <- apply(L, 2, sd) * sqrt(nrow(L))/sum(fit$fail)
  }
  else {
    cve <- as.double(mylossncvsurv(y, Y))/sum(fit$fail)
    cvse <- ncvreg:::se.ncvsurv(y, Y)/sum(fit$fail)
  }
  min <- which.min(cve)
  val <- list(cve = cve, cvse = cvse, fold = fold, lambda = lambda,
              fit = fit, min = min, lambda.min = lambda[min], null.dev = cve[1])
  if (returnY)
    val$Y <- Y
  structure(val, class = c("cv.ncvsurv", "cv.ncvreg"))
}
# environment(my.cv.ncvsurv) <- environment(cv.ncvsurv)



#' Modified \code{ncvreg:::ncvsurv}.
#'
#' modification of the \code{ncvsurv()} function (\code{ncvreg} internal)
#' with removal of the condition nlambda > 2
#'
#' @param X See \code{ncvreg:::ncvsurv}.
#' @param y See \code{ncvreg:::ncvsurv}.
#' @param penalty See \code{ncvreg:::ncvsurv}.
#' @param gamma See \code{ncvreg:::ncvsurv}.
#' @param alpha See \code{ncvreg:::ncvsurv}.
#' @param lambda.min See \code{ncvreg:::ncvsurv}.
#' @param nlambda See \code{ncvreg:::ncvsurv}.
#' @param lambda See \code{ncvreg:::ncvsurv}.
#' @param eps See \code{ncvreg:::ncvsurv}.
#' @param max.iter See \code{ncvreg:::ncvsurv}.
#' @param convex See \code{ncvreg:::ncvsurv}.
#' @param dfmax See \code{ncvreg:::ncvsurv}.
#' @param penalty.factor See \code{ncvreg:::ncvsurv}.
#' @param warn See \code{ncvreg:::ncvsurv}.
#' @param returnX See \code{ncvreg:::ncvsurv}.
#' @param ... See \code{ncvreg:::ncvsurv}.
#'
#' @importFrom ncvreg ncvsurv
#'
my.ncvsurv <- function (X, y, penalty = c("MCP", "SCAD", "lasso"),
                        gamma = switch(penalty, SCAD = 3.7, 3), alpha = 1,
                        lambda.min = ifelse(n > p, 0.001, 0.05), nlambda = 100,
                        lambda, eps = 1e-5, max.iter = 10000, convex = TRUE,
                        dfmax = p, penalty.factor = rep(1, ncol(X)),
                        warn = TRUE, returnX, ...){
  penalty <- match.arg(penalty)
  if (!inherits(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0 + ., data = X), silent = TRUE)
    if (inherits(tmp, "try-error"))
      stop("X must be a matrix or able to be coerced to a matrix",
           call. = FALSE)
  }
  if (storage.mode(X) == "integer")
    storage.mode(X) <- "double"
  if (!inherits(y, "matrix")) {
    tmp <- try(y <- as.matrix(y), silent = TRUE)
    if (inherits(tmp, "try-error"))
      stop("y must be a matrix or able to be coerced to a matrix",
           call. = FALSE)
    if (ncol(y) != 2)
      stop("y must have two columns for survival data: time-on-study and a censoring indicator",
           call. = FALSE)
  }
  if (typeof(y) == "integer")
    storage.mode(y) <- "double"
  if (typeof(penalty.factor) != "double")
    storage.mode(penalty.factor) <- "double"
  if (gamma <= 1 & penalty == "MCP")
    stop("gamma must be greater than 1 for the MC penalty",
         call. = FALSE)
  if (gamma <= 2 & penalty == "SCAD")
    stop("gamma must be greater than 2 for the SCAD penalty",
         call. = FALSE)

  if (alpha <= 0)
    stop("alpha must be greater than 0; choose a small positive number instead",
         call. = FALSE)
  if (length(penalty.factor) != ncol(X))
    stop("penalty.factor does not match up with X", call. = FALSE)
  if (any(is.na(y)) | any(is.na(X)))
    stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before passing X and y to ncvreg",
         call. = FALSE)

  tOrder <- order(y[, 1])
  yy <- as.double(y[tOrder, 1])
  Delta <- y[tOrder, 2]
  n <- length(yy)
  XX <- ncvreg::std(X[tOrder, , drop = FALSE])
  if (sys.nframe() > 1 && sys.call(-1)[[1]] == "local_mfdr")
    return(list(X = XX, time = yy, fail = Delta))
  ns <- attr(XX, "nonsingular")
  penalty.factor <- penalty.factor[ns]
  p <- ncol(XX)
  if (missing(lambda)) {
    lambda <- ncvreg:::setupLambdaCox(XX, yy, Delta, alpha, lambda.min,
                                      nlambda, penalty.factor)
    user.lambda <- FALSE
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }
  res <- .Call("cdfit_cox_dh", XX, Delta, penalty, lambda,
               eps, as.integer(max.iter), as.double(gamma), penalty.factor,
               alpha, as.integer(dfmax), as.integer(user.lambda | any(penalty.factor ==
                                                                        0)), as.integer(warn),
               PACKAGE = "ncvreg")

  b <- matrix(res[[1]], p, nlambda)
  loss <- -1 * res[[2]]
  iter <- res[[3]]
  Eta <- matrix(res[[4]], n, nlambda)
  ind <- !is.na(iter)
  b <- b[, ind, drop = FALSE]
  iter <- iter[ind]
  lambda <- lambda[ind]
  loss <- loss[ind]
  Eta <- Eta[, ind, drop = FALSE]
  if (warn & sum(iter) == max.iter)
    warning("Algorithm failed to converge for some values of lambda")
  convex.min <- if (convex)
    ncvreg:::convexMin(b, XX, penalty, gamma, lambda * (1 - alpha),
                       "cox", penalty.factor, Delta = Delta)
  else NULL
  beta <- matrix(0, nrow = ncol(X), ncol = length(lambda))
  bb <- b/attr(XX, "scale")[ns]
  beta[ns, ] <- bb
  offset <- -crossprod(attr(XX, "center")[ns], bb)
  varnames <- if (is.null(colnames(X)))
    paste("V", 1:ncol(X), sep = "")
  else colnames(X)
  dimnames(beta) <- list(varnames, ncvreg:::lamNames(lambda))
  val <- structure(list(beta = beta, iter = iter, lambda = lambda,
                        penalty = penalty, gamma = gamma, alpha = alpha,
                        convex.min = convex.min, loss = loss,
                        penalty.factor = penalty.factor, n = n, time = yy,
                        fail = Delta, order = tOrder),
                   class = c("ncvsurv", "ncvreg"))
  val$Eta <- sweep(Eta, 2, offset, "-")
  if (missing(returnX)) {
    if (utils::object.size(XX) > 1e+08) {
      warning("Due to the large size of X (>100 Mb), returnX has been turned off.\nTo turn this message off, explicitly specify returnX=TRUE or returnX=FALSE).")
      returnX <- FALSE
    }
    else {
      returnX <- TRUE
    }
  }
  if (returnX) {
    val$X <- XX
  }
  val
}
# environment(my.ncvsurv) <- environment(ncvsurv)
