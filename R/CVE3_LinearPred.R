#' Cross-valitation for cox regression using the linear predicor estimator
#'
#' @param data data contaning columns time and status
#' @param partition dataframe with 1 colunm
#' @param nfolds number of fold for CV
#'
#' @return numeric, cross-validation error
CVE_LinearPred <- function(data, partition, nfolds = 10) {
  X <- model.matrix(~part, data.frame(part = factor(partition[, 1])))

  my.cv.ncvsurv(
    X = X, y = as.matrix(data[, c("time", "status")]),
    penalty = "lasso", penalty.factor = rep(0, ncol(X)),
    lambda = 0, nfolds = nfolds, se = c("quick")
  )$cve
}



#' Cross-valitation for cox regression using the linear predicor estimator with
#' wrapper for warnings handling
#'
#' @param x list of 3 named elements : data (data contaning columns time and
#'   status), partition (dataframe with 1 colunm), nfolds (number of fold for
#'   CV)
#'
#' @return numeric, cross-validation error
#' @export
CVE_LP <- function(x){
  r <- withCallingHandlers({
    error_text <- NA
    list(value = CVE_LinearPred(partition = x$partition,
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
