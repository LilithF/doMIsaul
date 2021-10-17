#' Wrapper functions for multivariate imputation with survival data or
#' left-censored data
#'
#'
#' Performs imputation of the missing data using MICE and returns a list in the
#'  correct format for the \code{unsupMI()} and \code{seMIsupcox()}functions.
#'   \code{MImpute()} performs imputation for datasets with missing data only.
#'   \code{MImpute_surv()} performs imputation for a dataset with survival data.
#'   The Nelson Aalen estimator is calculated and used as predictor in the
#'   imputation, Time is not used as predictor.
#'   \code{MImpute_lcens()} performs imputation for a dataset with left-censored
#'    data. Note that with \code{MImpute_lcens()} pmm imputation is
#'   performed for variables not affected by left-censoring.
#'
#'
#' @param data Dataframe with incomplete data. (for \code{MImpute_lcens()}, with
#'   \code{NA} for both missing and left-censored data).
#' @param mi.m Number of imputations to perform.
#' @param method Optional.  single string, or a vector of strings specifying
#'    the imputation method to be used for each column in data
#'  (passed to \code{mice()}). If \code{NULL} default mice setting are used.
#' @param predMat Optional. supply a \code{predictorMatrix} (passed to
#' \code{mice()}). If \code{NULL} default mice setting are used.
#' @param maxit passed to \code{mice()}.
#' @param return.midsObject Boolean
#' @param time.status.names Names of the variables for time and status
#'  (in that order).
#' @param data.lod Dataframe containing indicators of which observation are
#'   left-censored (censoring value for such observations and any other values
#'   for not censored observations). The colnames should correspond to variables
#'   in \code{data}. The variables that are left-censored are
#'   thus given in \code{data} (with left-censored data as \code{NA}) and in
#'   \code{data.lod} with random values for observed data and the LOD for
#'   left-censored data. Note that if the data are to be logged
#'   (\code{is.numeric(mice.log)}), only the argument \code{data} will be
#'   logged,  therefore, the LOD values given here should be given as
#'   \code{log(LOD)} with the correct number of decimals:
#'   \code{round(log(LOD), mice.log)}.
#' @param standards  Dataframe of 1 row containing the LOD values (not logged,
#'   whatever the value for \code{mice.log}).
#' @param mice.log set to \code{FALSE} if the imputation should be performed on
#'  unlogged data. Otherwise, number of decimal to save after taking the log of
#'  data (should be 10 unless for specific reasons) ; in that case the data will
#'  be unlogged after imputation.
#' @param var.log names of variables to log if \code{mice.log} is numeric. If
#'   \code{NULL}, all variables but those in  \code{time.status.names} will be
#'    logged.
#'
#'
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
#' cancer.imp <- MImpute(cancer[, -c(1:3)], 2)
#'
MImpute <- function(data, mi.m, method = NULL, predMat = NULL, maxit = 10,
                    return.midsObject = FALSE){

  tmp <- suppressWarnings(mice::mice(data, m = 1, maxit = 0, print = FALSE))
  if(is.null(method)){
    my.method <- tmp$method
  } else {
    my.method <- method
  }

  if(is.null(predMat)){
    my.predictorMatrix <- tmp$predictorMatrix
  } else {
    my.predictorMatrix <- predMat
  }

  res <- mice::mice(data, m = mi.m, method = my.method,
                    predictorMatrix = my.predictorMatrix,
                    maxit = maxit, print = FALSE)
  imp.ret <- mice::complete(res, "all")

  if(return.midsObject){
    return(list(
      imputed.data = imp.ret,
      mids.obj = res))
  } else {
    return(imp.ret)
  }
}

#' @rdname MImpute
#' @export
#'
#' @examples
#' ## MImpute_surv
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' cancer.imp <- MImpute_surv(cancer, 1)
#'
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




#' @rdname MImpute
#' @export
#'
#' @examples
#' ## MImpute_lcens
#' toy <- iris[, 1:4]
#' # censor on variables 3 and 4, with LOD at quantile .1 and .2.
#' LODs <- toy[1, ]
#' LODs[1, ] <-c(NA, NA, quantile(toy[,3], .2), quantile(toy[,4], .1))
#' # Censor indicator
#' Censored <- data.frame(Petal.Length = runif(150, 50,60),
#'                        Petal.Width = runif(150, 50,60))
#' Censored[toy[,3] < LODs[1, 3], 1] <- LODs[1, 3]
#' Censored[toy[,4] < LODs[1, 4], 2] <- LODs[1, 4]
#' # NA for censored data
#' toy[toy[,3] < LODs[1, 3], 3] <- NA
#' toy[toy[,4] < LODs[1, 4], 4] <- NA
#' # Additional missing data
#' toy[sample(1:nrow(toy), 30), 1] <- NA
#' toy[sample(1:nrow(toy), 30), 3] <- NA
#' toy[sample(1:nrow(toy), 30), 4] <- NA
#'
#' toy.imp <- MImpute_lcens(data = toy, data.lod = Censored, standards = LODs,
#'                          mi.m = 1, mice.log = FALSE)
MImpute_lcens <- function(data, data.lod, standards, mi.m, mice.log = 10,
                          maxit = 10, return.midsObject = FALSE){

  if (is.numeric(mice.log)){
    data.cens <- log(data)
    elm <- "round(log("
    elm2 <- paste("),", mice.log, ")")
  } else {
    data.cens <- data
    elm2 <- elm <- ""
  }

  if (ncol(data.lod) > 0)
    colnames(data.lod) <- paste0(colnames(data.lod), "lod")
  tmp <- data.frame(cbind(data.cens, data.lod))

  my.method <- sapply(colnames(tmp), function(i){
    if (sum(is.na(tmp[, i])) > 0){
      if (is.element(paste0(i, "lod"), colnames(tmp))){
        "cens"
      } else {
        "pmm"
      }
    } else {
      ""
    }

  }, USE.NAMES = TRUE)

  my.blots <- sapply(names(my.method), function(i){
    if (my.method[i] == "cens") {
      list(lod.j = paste(elm, unname(standards[1, i]), elm2))
    } else {
      list()
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  res <- mice::mice(tmp, m = mi.m, method = my.method,
                    maxit = maxit, blots = my.blots, print = FALSE)

  imp.ret <- mice::complete(res, "all")

  if (is.numeric(mice.log)){
    imp.ret <- sapply(imp.ret, exp, USE.NAMES = TRUE, simplify = FALSE)
  }

  imp.ret <- sapply(imp.ret, function(dtemp, nc = ncol(data)){
    dtemp[, 1:nc]
  }, USE.NAMES = TRUE, simplify = FALSE)


if(return.midsObject){
  imp2 <- mice::complete(res, "long", include = TRUE)
  if (is.numeric(mice.log)){
    imp2[, -c(1:2)] <- exp(imp2[, -c(1:2)])
  }
  imp2 <- imp2[, 1:ncol(data)]
  MIDS <- mice::as.mids(imp2)

  return(list(
    imputed.data = imp.ret,
    mids.obj = MIDS))
  } else {
    return(imp.ret)
  }

}


#' @rdname MImpute
#' @export
#'
#' @examples
#' ## MImpute_lcenssurv
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' toy2 <- cancer[, -1]
#' # censor on variables age and meal.cal, with LOD at quantile .1 and .2.
#' LODs <- toy2[1, ]
#' LODs[1, ] <-c(NA, NA, quantile(toy2[, "age"], .2, na.rm = TRUE), NA,  NA,
#'               NA, NA, quantile(toy2[, "meal.cal"], .1, na.rm = TRUE), NA)
#' # Censor indicator
#' Censored <- data.frame(age = runif(nrow(toy2), 300,400),
#'                        meal.cal = runif(nrow(toy2), 50,60))
#' Censored[toy2[, "age"] < LODs[1, "age"], "age"] <- LODs[1, "age"]
#' Censored[!is.na(toy2[, "meal.cal"]) &
#'            toy2[, "meal.cal"] < LODs[1, "meal.cal"], "meal.cal"] <-
#'  LODs[1, "meal.cal"]
#' # NA for censored data
#' toy2[toy2[, "age"] < LODs[1, "age"], "age"] <- NA
#' toy2[!is.na(toy2[, "meal.cal"]) &
#'       toy2[, "meal.cal"] < LODs[1, "meal.cal"],"meal.cal"] <- NA
#' # Additional missing data
#' toy2[sample(1:nrow(toy2), 30), 6] <- NA
#' toy2[sample(1:nrow(toy2), 30), 3] <- NA
#' toy2[sample(1:nrow(toy2), 30), 4] <- NA
#' toy2$sex <- factor(toy2$sex)
#' toy2$ph.ecog <- factor(toy2$ph.ecog)
#'
#' toy2.imp <- MImpute_lcenssurv(
#'   data = toy2, mi.m = 1, data.lod = Censored, standards = LODs,
#'   mice.log = FALSE)
#'
MImpute_lcenssurv <- function(
  data, mi.m, time.status.names = c("time", "status"),
  data.lod, standards, mice.log = 10, var.log = NULL, maxit = 10,
  return.midsObject = FALSE){


  # prepare data/parameters for censored imputation: log of not some data
  if (is.numeric(mice.log)){
    if(is.null(var.log)){
      var.log <- setdiff(colnames(data), time.status.names)
    }
    if(sum(var.log %in% colnames(data)) < 1) {
      stop("if 'mice.log' is numeric, 'var.log' must be either NULL or a vector
           of names included in 'colnames(data)'")
    }
    data.cens <- data
    data.cens[, var.log] <- log(data[, var.log])
    elm <- "round(log("
    elm2 <- paste("),", mice.log, ")")

  } else {

    data.cens <- data
    elm2 <- elm <- ""

  }

  # generate nelsonaalen estimator
  dtmp <- data.cens
  dtmp$time <- data.cens[,  time.status.names[1]]
  dtmp$status <- data.cens[,  time.status.names[2]]

  data.cens$H0 <- mice::nelsonaalen(dtmp, "time", "status")


  # include censor indicators
  if (ncol(data.lod) > 0)
    colnames(data.lod) <- paste0(colnames(data.lod), "lod")
  data.to.imp <- data.frame(cbind(data.cens, data.lod))

  tmp <-
    suppressWarnings(mice::mice(data.to.imp, m = 1, maxit = 0, print = FALSE))

  # Prepare method parameter
  my.method <- tmp$method
  my.method <- sapply(names(my.method), function(i){

    if (is.element(paste0(i, "lod"), colnames(data.to.imp))){
      "cens"
    } else {
      unname(my.method[i])
    }

  }, USE.NAMES = TRUE)
  my.method[c(time.status.names, "H0")] <- ""

  # Prepare blot parameter
  my.blots <- sapply(names(my.method), function(i){
    if (my.method[i] == "cens") {
      list(lod.j = paste(elm, unname(standards[1, i]), elm2))
    } else {
      list()
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  # Prepare predictorMatrix parameter
  my.predictorMatrix <- tmp$predictorMatrix
  my.predictorMatrix[, colnames(my.predictorMatrix) %in%
                       time.status.names[1]] <- 0


  #  maxit parameter
  # If missing for >1 var maxit iterations, else 1 is enough
  if(sum(sapply(data.to.imp, function(x){any(is.na(x))},
                simplify = TRUE, USE.NAMES = TRUE)) > 1){
    max.it <- maxit
  } else {
    max.it <- 1
  }


  # Impute
  res <- mice::mice(data.to.imp, m = mi.m, method = my.method,
                    predictorMatrix = my.predictorMatrix,
                    maxit = max.it, blots = my.blots, printFlag = FALSE)

  imp <- mice::complete(res, "all")

  # unlog if necessary
  if (is.numeric(mice.log)){
    imp <- sapply(imp, function(x){
      x[, var.log] <- exp(x[, var.log])
      x
    }, USE.NAMES = TRUE, simplify = FALSE)
  }

  # remove added colunms
  imp <- sapply(imp, function(x){
    x[, !colnames(x) %in% c("H0", paste0(colnames(data), "lod"))]
  }, USE.NAMES = TRUE, simplify = FALSE)


  if(return.midsObject){
    imp2 <- mice::complete(res, "long", include = TRUE)
    if (is.numeric(mice.log)){
      imp2[, var.log] <- exp(imp2[, var.log])
    }
    imp2 <- imp2[, !colnames(imp2) %in% c("H0", paste0(colnames(data), "lod"))]
    mids.obj <- mice::as.mids(imp2)

    return(list(
      imputed.data = imp,
      mids.obj = mids.obj))
  } else {
    return(imp)
  }
}

