#' Apply format to p-values
#'
#' @keywords internal
#'
#' @param p digit. The p-value to format
#' @param text boolean. Set to \code{TRUE}to display \code{"p="}, to
#'   \code{FALSE} to display only the value.
#'
#' @return pvalue
formatpv <- function(p, text = FALSE) {

  if(p < 0.0001) {return("<0.0001")}
  if(p >= 0.0001 & p < 0.00095) {
    ifelse(text == FALSE,
           return(sprintf("%.4f", p)),
           return(paste("=", sprintf("%.4f", p), sep = ""))
    )
  }

  if(p >= 0.00095 & p <= 0.0095) {
    ifelse(text == FALSE,
           return(as.character(signif(p, 1))),
           return(paste("=", as.character(signif(p, 1)), sep = ""))
    )
  }

  if(p > 0.0095 & p < 0.0995) {
    ifelse(text == FALSE,
           return(sprintf("%.3f", signif(p, 2))),
           return(paste("=", sprintf("%.3f", signif(p, 2)), sep = ""))
    )
  }

  if(p >= 0.0995) {
    ifelse(text == FALSE,
           return(sprintf("%.2f", signif(p, 2))),
           return(paste("=", sprintf("%.2f", signif(p, 2)), sep = ""))
    )
  }

}
