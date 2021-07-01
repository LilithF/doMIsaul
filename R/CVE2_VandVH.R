#' Cross-validation for cox regression using Verweij and Van Houwelingen method
#'
#' @param data data containing columns time and status.
#' @param partition dataframe with 1 column.
#' @param nfolds number of fold for CV.
#'
#' @return numeric, cross-validation error
#' @examples
#' data(cancer, package = "survival")
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = TRUE)
#' doMIsaul:::cve_VVH(cancer, part, 10)
cve_VVH <- function(data, partition, nfolds = 10){

  X <- model.matrix(~ part, data.frame(part = factor(partition[, 1])))

  my.cv.glmnet(
    x = X,
    y = as.matrix(data[, c("time", "status")]),
    family = "cox",
    lambda = 0,
    type.measure = "deviance",
    nfolds = nfolds,
    grouped = TRUE,
    thresh = 1e-5
  )$cvm

}



#' Cross-validation for cox regression using Verweij and Van Houwelingen
#' estimator with wrapper for warnings handling
#'
#' @param x list of 3 named elements : data (data containing columns time and
#'   status), partition (dataframe with 1 column), nfolds (number of fold for
#'   CV).
#'
#' @return numeric, cross-validation error
#' @export
#' @examples
#' data(cancer, package = "survival")
#' cancer$status  <- cancer$status - 1
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = TRUE)
#' CVE_VandVH(list(data = cancer, partition = part, nfolds = 10))
CVE_VandVH <- function(x){
  r <- withCallingHandlers({
    error_text <- NA
    list(value = cve_VVH(partition = x$partition,
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
