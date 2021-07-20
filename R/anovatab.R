#' Table of ANOVA test for several explanatory variables
#'
#' Performs the function \code{aov()} for the \code{main.var} and \code{var}.
#'
#' @keywords internal
#'
#' @param data The database.
#' @param main.var string. The categorical variable against the variable
#'   will be compared.
#' @param var strings. The categorical variable to compare with.
#' @param nb.dec digit. Number of decimals for the mean and quantiles
#'
#' @return table with mean and q1 q3 for each level and p-value of ANOVA test.
anovatab <- function(data, main.var, var, nb.dec = 1){

  ret <- as.data.frame(matrix(NA, nrow = 1, ncol = nlevels(data[, main.var])+2))

  colnames(ret) <- c("Parameter", levels(data[, main.var]), "pvalue")

  ret[1,1] <- var

  ret[1, -c(1, ncol(ret))] <-
    sapply(levels(data[, main.var]), function(lev){

      m <- mean(data[data[, main.var] %in% lev, var], na.rm = TRUE)

      qq <- quantile(data[data[, main.var] %in% lev, var],
                     c(.25, .75), na.rm = TRUE)

      paste0(round(m, nb.dec), " [", round(qq[1], nb.dec),
             " ; ", round(qq[2], nb.dec), "]")

    })


  ret$pvalue[nrow(ret)] <-
    summary(stats::aov(
      as.formula(paste(var, "~", main.var)), data)
      )[[1]]$`Pr(>F)`[1]

  ret
}
