#' Cross-validation for cox regression using the basic approach
#'
#' @param data data containing columns time and status.
#' @param partition dataframe with 1 column.
#' @param nfolds number of fold for CV.
#'
#' @return numeric, cross-validation error
#' @examples
#' data(cancer, package = "survival")
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = TRUE)
#' doMIsaul:::cve_bas(cancer,part, 10)
cve_bas <- function(data, partition, nfolds = 10){
  X <- model.matrix(~ part, data.frame(part = factor(partition[, 1])))

  my.cv.glmnet(
    x = X,
    y = as.matrix(data[, c("time", "status")]),
    family = "cox",
    lambda = 0,
    type.measure = "deviance",
    nfolds = nfolds,
    grouped = FALSE,
    thresh = 1e-5
  )$cvm

}


#' Cross-validation for cox regression using the basic approach
#' estimator with wrapper for warnings handling
#'
#' @param x list of 3 named elements : \code{data} (data containing columns time
#'  and status), \code{partition} (dataframe with 1 column), \code{nfolds}
#'   (number of fold for cross-validation).
#'
#' @return numeric, cross-validation error
#' @export
#' @examples
#' data(cancer, package = "survival")
#' part <- data.frame(Cl= factor(cancer[, "sex"]), stringsAsFactors = TRUE)
#' CVE_basic(list(data = cancer, partition = part, nfolds = 10))
CVE_basic <- function(x){
  r <- withCallingHandlers({
    error_text <- NA
    list(value = cve_bas(partition = x$partition,
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
