#' Wrapper to evaluate time dependent AUC
#'
#' @keywords internal
#'
#' @param data  dataframe containing time and status information, as well as
#'  marker
#' @param predictor  \code{colname} of the marker to use to calculate AUC
#' @param time.vect vector of time points to consider
#'
#' @return AUC at \code{max(time.vect)} and median AUC along \code{time.vect}.
Extract_AUC <- function(data, predictor, time.vect){

  requireNamespace("survival", quietly = FALSE)

  AUCs <- timeROC::timeROC(data$time, delta = data$status,
                           marker = data[, predictor],
                           cause = 1, times = time.vect)
  return(list(AUC.Tmax = unname(AUCs$AUC[length(time.vect)]),
              AUC.med = median(AUCs$AUC, na.rm = TRUE)))
}
