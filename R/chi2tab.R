#' Table of \code{chisq.test()} test for several explanatory variables
#'
#' Performs the function \code{chisq.test()} for the \code{main.var} and
#'  \code{var}.
#'
#' @keywords internal
#'
#' @param data The database.
#' @param main.var string. The categorical variable against the variable
#'   will be compared.
#' @param var string. The categorical variable to compare with.
#' @param nb.dec digit. Number of decimals for the percentage.
#'
#' @return table with n (percentage) per level and p-value of \code{chi2} test.
chi2tab <- function(data, main.var, var, nb.dec = 1){

  freq <- as.data.frame(
    xtabs(as.formula(paste("~", main.var, "+", var)), data)
    )

  ret <- reshape2::dcast(freq,
                         formula = as.formula(paste(var, "~", main.var)),
                         value.var = "Freq")

  ret[, -1] <- apply(ret[, -1], 2, function(x){

    per <- paste0(round(100*x/sum(x), nb.dec), "%")
    paste0(x, " (", per, ")")

  })

  ret$pvalue[nrow(ret)] <- chisq.test(data[, main.var], data[, var])$p.value

  ret <- data.frame(Parameter = NA, ret, stringsAsFactors = TRUE)
  ret[1,1] <- var
  colnames(ret)[2:(ncol(ret)-1)] <- c("Levels", levels(data[, main.var]))
  ret

}
