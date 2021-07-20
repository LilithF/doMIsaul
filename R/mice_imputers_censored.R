#' Impute left censored data with MICE
#'
#' Function from \code{Lapidus et al.} for imputing left-censored data with mice
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the the subset
#'   \code{y[ry]} of elements in y to which the imputation model is fitted.
#'   The \code{ry} generally distinguishes the observed (\code{TRUE}) and
#'   missing values (\code{FALSE}) in y.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors
#'   for \code{y}. Matrix \code{x} may have no missing values.
#' @param lod.j censoring value.
#' @param lod.name suffix name used for the censored variable.
#' @param REDRAW Boolean indicating whether values should be redrawn if some
#'   are over the censoring limit
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#'   indicates locations in \code{y} for which imputations are created.
#' @param ... Other named arguments.
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#'   \code{sum(wy)}.
#' @export
mice.impute.cens <- function(y, ry, x, lod.j, lod.name = "lod",
                             REDRAW = FALSE, wy = NULL, ...) {

  lod.j <- eval(parse(text = lod.j))
  if (is.null(wy))
    wy <- !ry
  x <- as.matrix(x)
  LOD.var <- grep(lod.name, colnames(x))
  xx <- x[, paste0(setdiff(gsub(lod.name, "", colnames(x)[LOD.var]),
                           colnames(x)[-LOD.var]), lod.name)]
  x <- cbind (1, as.matrix(x[, -LOD.var]))

  yy <- y
  yy[xx == lod.j] <- lod.j
  ryy <- ry
  ryy[xx == lod.j] <- TRUE
  doMI.cens.draw3 <- get(".cens.draw3", envir = asNamespace("doMIsaul"),
                         inherits = FALSE)
  parm <- doMI.cens.draw3(y = yy, ry = ryy, x = x, lod.j = lod.j)
  wyy <- wy[wy]
  wyy[xx[wy] != lod.j] <- FALSE
  .draw <- as.numeric (x[wy, ] %*% parm$beta +
                         rnorm(sum(wy)) * parm$sigma)
  .draw[wyy] <- truncnorm::rtruncnorm(
    sum(wyy), a = -Inf, b = lod.j,
    mean = x[wy & xx == lod.j, ] %*% parm$beta,
    sd = parm$sigma
  )
  n.redraw <- sum (.draw[wyy] > lod.j)
  if (REDRAW)
    while (n.redraw) {
      x.mis <- x[wy, ]
      x.mis <- x.mis[wyy, ]
      .draw[(.draw > lod.j) &
              wyy] <-
        as.numeric(x.mis[(.draw[wyy] > lod.j), ] %*% parm$beta +
                     rnorm(n.redraw) * parm$sigma)
      n.redraw <- sum(.draw[wyy] > lod.j)
    }
  .draw <- as.matrix(.draw)
  return(.draw)
}
# attributes(mice.impute.cens) <- attributes(mice.impute.norm)
environment(mice.impute.cens) <- environment(mice::mice.impute.norm)



#' Base function for imputing left censored data with MICE
#'
#'  from \code{Lapidus et al.}
#'
#'
#' @keywords internal
#'
#'
#' @param y Vector to be imputed
#' @param ry Logical vector of length \code{length(y)} indicating the the subset
#'   \code{y[ry]} of elements in y to which the imputation model is fitted.
#'   The \code{ry} generally distinguishes the observed (\code{TRUE}) and
#'   missing values (\code{FALSE}) in y.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors
#'   for \code{y}. Matrix \code{x} may have no missing values.
#' @param lod.j censoring value.
#' @param ... Other named arguments.
#'
#' @return parameters
.cens.draw3 <- function(y, ry, x, lod.j=lod.j, ...) {

  x <- as.data.frame(x)
  x <- x[ry, ]
  y <- y[ry]

  form <-
    as.formula(paste("y", paste(names(x)[-1], collapse = " + "), sep = " ~ "))

  fit3 <- censReg::censReg(form, left = lod.j, data = x)

  draw <- MASS::mvrnorm(1, mu = fit3$estimate, Sigma = solve(-fit3$hessian))
  parm <- list(beta = as.numeric(draw[-length(draw)]),
               sigma = exp(as.numeric(draw[length(draw)])))

  return(parm)
}
