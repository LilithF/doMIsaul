#' Cross-valitation for cox regression using Verweij and Van Houwelingen method
#'
#' @param data data contaning columns time and status
#' @param partition dataframe with 1 colunm
#' @param nfolds number of fold for CV
#'
#' @return numeric, cross-validation error
CVE1_VandVH <- function(data, partition, nfolds = 10){

  X <- model.matrix(~ part, data.frame(part = factor(partition[, 1])))

  my.cv.glmnet(
    x = X,
    y = as.matrix(data[, c("time", "status")]),
    family = "cox",
    lambda = 0,
    type.measure = "deviance",
    nfolds = nfolds,
    grouped = T,
    thresh = 1e-5
  )$cvm

}



#' Cross-valitation for cox regression using Verweij and Van Houwelingen
#' estimator with wrapper for warnings handling
#'
#' @param x list of 3 named elements : data (data contaning columns time and
#'   status), partition (dataframe with 1 colunm), nfolds (number of fold for
#'   CV)
#'
#' @return numeric, cross-validation error
#' @export
CVE_VandVH <- function(x){
  r <- withCallingHandlers({
    error_text <- NA
    list(value = CVE1_VandVH(partition = x$partition,
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
