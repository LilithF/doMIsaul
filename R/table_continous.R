#' Display table with comparison of the partition with continuous variables.
#'
#' @param data The dataset.
#' @param partition.name string. Name of the partition (in data). The partition
#'   variable should be a factor.
#' @param vars.cont vector of strings. variables to compare to (continuous
#'   only).
#' @param vars.cont.names Optional. Names for displaying the continuous
#'   variables. (in the same order than \code{vars.cont})
#' @param na.value Value to use for the empty cases (e.g. \code{""} or
#'    \code{NA}).
#' @param nb.dec digit. Number of decimals for the mean and quartile values.
#' @param text.pval boolean. Set to \code{TRUE}to display \code{"p="}, to
#'   \code{FALSE} to display only the value.
#'
#' @return table with mean and Q1 Q3 values per level of the partition and
#'   ANOVA test p-values.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- factor(cancer$status)
#' table_continuous(data = cancer, partition.name = "status",
#'                   vars.cont = c("age", "meal.cal", "wt.loss"))
table_continuous <- function(data, partition.name, vars.cont,
                             vars.cont.names = NULL, na.value = "",
                             nb.dec = 1, text.pval = FALSE){

  tab <- do.call(rbind,
                 lapply(vars.cont, anovatab,
                        data = data, main.var = partition.name))

  tab$pvalue <- sapply(tab$pvalue, formatpv, text = text.pval)

  if(!is.null(vars.cont.names)){

    tab$Parameter <- plyr::mapvalues(tab$Parameter,
                               vars.cont,
                               vars.cont.names,
                               warn_missing = FALSE)
  }

  tab[is.na(tab)] <- na.value

  colnames(tab)[-c(1, ncol(tab))] <-
    paste0(partition.name, ": ", levels(factor(data[, partition.name])))

  tab

}
