#' Cross-validation for cox regression using the linear predictor estimator
#'
#' @keywords internal
#'
#' @param data data containing columns time and status.
#' @param partition dataframe with 1 column containing the partition labels.
#' @param nfolds number of fold for CV.
#'
#' @return numeric, cross-validation error
cve_LinearPred <- function(data, partition, nfolds = 10) {
  X <- model.matrix(~part, data.frame(part = factor(partition[, 1])))

  ncvreg::cv.ncvsurv(
    X = X, y = as.matrix(data[, c("time", "status")]),
    penalty = "lasso", penalty.factor = rep(1, ncol(X)),
    nlambda = 2, lambda.min = 0, nfolds = nfolds, se = c("quick"),
    eps = 1e-05
  )$cve[2]
}



#' Cross-validation for cox regression using the linear predictor estimator with
#' wrapper for warnings handling
#'
#' @param x list of 3 named elements : \code{data} (data containing columns time
#'  and status), \code{partition} (dataframe with 1 column), \code{nfolds}
#'   (number of fold for cross-validation).
#'
#' @return numeric, cross-validation error
#' @export
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = TRUE)
#' CVE_LP(list(data = cancer, partition = part, nfolds = 10))
CVE_LP <- function(x){
  r <- withCallingHandlers({
    error_text <- NA
    list(value = cve_LinearPred(partition = x$partition,
                                data = x$data,
                                nfolds = x$nfolds),
         error = error_text)
  },
  warning = function(e) {
    error_text <<- 1
    invokeRestart("muffleWarning")
  }
  )

  return(r)
}
