#' Cross-valitation for cox regression using the linear predicor estimator
#'
#' @param data data contaning columns time and status
#' @param partition dataframe with 1 colunm
#' @param nfolds number of fold for CV
#'
#'
#' @return numeric, cross-validation error
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = T)
#' doMIsaul:::cve_LinearPred(cancer,part, 10)
cve_LinearPred <- function(data, partition, nfolds = 10) {
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
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = T)
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
