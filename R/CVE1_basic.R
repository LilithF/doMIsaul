#' Cross-valitation for cox regression using the basic approach
#'
#' @param data data contaning columns time and status
#' @param partition dataframe with 1 colunm
#' @param nfolds number of fold for CV
#'
#' @return numeric, cross-validation error
#' @examples part <- data.frame(x= as.numeric(iris[, 5]))
#' CVE1_basic(iris[, 1:4],part, 10)
CVE1_basic <- function(data, partition, nfolds = 10){
  X <- model.matrix(~ part, data.frame(part = factor(partition[, 1])))

  my.cv.glmnet(
    x = X,
    y = as.matrix(data[, c("time", "status")]),
    family = "cox",
    lambda = 0,
    type.measure = "deviance",
    nfolds = nfolds,
    grouped = F,
    thresh = 1e-5
  )$cvm

}


#' Cross-valitation for cox regression using the basic approach
#' estimator with wrapper for warnings handling
#'
#' @param x list of 3 named elements : data (data contaning columns time and
#'   status), partition (dataframe with 1 colunm), nfolds (number of fold for
#'   CV)
#'
#' @return numeric, cross-validation error
#' @examples part <- data.frame(x= as.numeric(iris[, 5]))
#' CVE_basic(list(data = iris[, 1:4], partition = part, nfolds = 10))
CVE_basic <- function(x){
  r <- withCallingHandlers({
    error_text <- NA
    list(value = CVE1_basic(partition = x$partition,
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
