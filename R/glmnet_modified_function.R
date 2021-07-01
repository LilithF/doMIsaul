#' Modified \code{glmnet:::cv.glmnet}.
#'
#' Modification of the cv.glmnet function (glmnet package version 2.0-18)
#'  with removal of the condition lenght(lamba) > 2
#'
#' @param x See \code{glmnet:::cv.glmnet}.
#' @param y See \code{glmnet:::cv.glmnet}.
#' @param weights See \code{glmnet:::cv.glmnet}.
#' @param offset See \code{glmnet:::cv.glmnet}.
#' @param lambda See \code{glmnet:::cv.glmnet}.
#' @param type.measure See \code{glmnet:::cv.glmnet}.
#' @param nfolds See \code{glmnet:::cv.glmnet}.
#' @param foldid See \code{glmnet:::cv.glmnet}.
#' @param alignment See \code{glmnet:::cv.glmnet}.
#' @param grouped See \code{glmnet:::cv.glmnet}.
#' @param keep See \code{glmnet:::cv.glmnet}.
#' @param parallel See \code{glmnet:::cv.glmnet}.
#' @param ... See \code{glmnet:::cv.glmnet}.
#'
#' @import glmnet
#'
#'
my.cv.glmnet <- function(
  x, y, weights = NULL, offset = NULL, lambda = NULL,
  type.measure = c("default", "mse", "deviance","class", "auc", "mae", "C"),
  nfolds = 10, foldid = NULL, alignment = c("lambda", "fraction"),
  grouped = TRUE, keep = FALSE, parallel = FALSE,
  gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 0, ...) {

  requireNamespace("glmnet", quietly = FALSE)
  type.measure = match.arg(type.measure)
  alignment = match.arg(alignment)

  if (!is.null(lambda) && alignment == "fraction") {
    warning("fraction of path alignment not available if lambda given as argument; switched to alignment=`lambda`")
    alignment = "lambda"
  }
  N = nrow(x)
  if (is.null(weights))
    weights = rep(1, N)
  else weights = as.double(weights)
  y = drop(y)
  cv.call = glmnet.call = match.call(expand.dots = TRUE)
  which = match(c("type.measure", "nfolds", "foldid",
                  "grouped", "keep"), names(glmnet.call), FALSE)
  if (any(which))
    glmnet.call = glmnet.call[-which]
  glmnet.call[[1]] = as.name("glmnet")
  if (glmnet.control()$itrace)
    trace.it = 1
  else {
    if (trace.it) {
      glmnet.control(itrace = 1)
      on.exit(glmnet.control(itrace = 0))
    }
  }
  if (is.null(foldid))
    foldid = sample(rep(seq(nfolds), length = N))
  else nfolds = max(foldid)
  if (nfolds < 3)
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  if (relax)
    cv.relaxed.raw(x, y, weights, offset, lambda, type.measure,
                   nfolds, foldid, alignment, grouped, keep, parallel,
                   trace.it, glmnet.call, cv.call, gamma, ...)
  else cv.glmnet.raw(x, y, weights, offset, lambda, type.measure, nfolds,
                     foldid, alignment, grouped, keep, parallel, trace.it,
                     glmnet.call, cv.call, ...)
  }

environment(my.cv.glmnet) <- environment(glmnet::cv.glmnet)

