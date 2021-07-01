#' Wrapper function for multivariate imputation with survival data
#'
#'
#' Performs imputation of the missing data using MICE for a dataset with
#' survival data. The Nelson Aalen estimator is calculated and used as predictor
#' in the imputation, Time is not used as predictor.
#'
#'
#' @param data Dataframe with incomplete data.
#' @param mi.m Number of imputations to perform.
#' @param time.status.names Names of the variables for time and status
#'  (in that order)
#' @param return.midsObject Boolean
#'
#' @return If \code{return.midsObject == FALSE} a list of size mi.m, containing
#'   the imputed datasets. If \code{return.midsObject == TRUE} a list of 2, the
#'   first element (\code{imputed.data}) being the list of size mi.m as
#'   described in the previous sentence, the 2nd element (\code{mids.obj})
#'   containing the \code{mids} object as returned by \code{mice()}
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' cancer.imp <- MImpute_surv(cancer, 3)
MImpute_surv <- function(data, mi.m, time.status.names = c("time", "status"),
                         return.midsObject = FALSE){

  dtmp <- data
  dtmp$time <- data[,  time.status.names[1]]
  dtmp$status <- data[,  time.status.names[2]]

  data$H0 <- mice::nelsonaalen(dtmp, "time", "status")
  tmp <- suppressWarnings(mice::mice(data, m = 1, maxit = 0, print = FALSE))
  my.method <- tmp$method
  my.predictorMatrix <- tmp$predictorMatrix
  my.method[c(time.status.names, "H0")] <- ""
  my.predictorMatrix[, colnames(my.predictorMatrix) %in%
                       time.status.names[1]] <- 0

  # If missing for >1 var 10 iterations, else 1 is enough
  if(sum(sapply(data, function(x){any(is.na(x))},
                simplify = TRUE, USE.NAMES = TRUE)) > 1){
    max.it <- 10
  } else {
    max.it <- 1
  }
  # set.seed(1)
  res <- mice::mice(data, m = mi.m, method = my.method,
              predictorMatrix = my.predictorMatrix,
              maxit = max.it, printFlag = FALSE)
  imp <- mice::complete(res, "all")
  imp <- lapply(imp, function(x){
    x[, !colnames(x) %in% c("H0")]
  })


  if(return.midsObject){
    imp2 <- mice::complete(res, "long", include = TRUE)
    imp2 <- imp2[, !colnames(imp2) %in% "HO"]
    mids.obj <- mice::as.mids(imp2)

    return(list(
      imputed.data = imp,
      mids.obj = mids.obj))
  } else {
    return(imp)
  }
}
