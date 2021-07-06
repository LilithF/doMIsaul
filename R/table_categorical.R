#' Display table with comparison of the partition with categorical variables.
#'
#' @param data The dataset.
#' @param partition.name string. Name of the partition (in data). The partition
#'   variable should be a factor.
#' @param vars.cat vector of strings. variables to compare to (categorical
#'   only).
#' @param vars.cat.names Optional. Names for displaying the categorical
#'   variables. (in the same order than \code{vars.cat})
#' @param na.value Value to use for the empty cases (e.g. \code{""} or
#'    \code{NA}).
#' @param nb.dec digit. Number of decimals for the percentage.
#' @param text.pval boolean. Set to \code{TRUE}to display \code{"p="}, to
#'   \code{FALSE} to display only the value.
#'
#' @return table with n and percentage values per level of the partition and
#'   chi square test p-values.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- factor(cancer$status)
#' table_categorical(data = cancer, partition.name = "status",
#'                   vars.cat = c("sex", "ph.ecog"))
table_categorical <- function(data, partition.name, vars.cat,
                              vars.cat.names = NULL, na.value = "",
                              nb.dec = 1, text.pval = FALSE){

  tab <- do.call(rbind,
                 lapply(vars.cat, chi2tab,
                        data = data, main.var = partition.name, nb.dec = nb.dec)
                 )

  tab$pvalue[!is.na(tab$pvalue)] <-
    sapply(tab$pvalue[!is.na(tab$pvalue)], formatpv, text = text.pval)

  if(!is.null(vars.cat.names)){

    tab$Parameter <- plyr::mapvalues(tab$Parameter,
                               vars.cat,
                               vars.cat.names,
                               warn_missing = FALSE)
  }

  tab[is.na(tab)] <- na.value

  colnames(tab)[-c(1:2, ncol(tab))] <-
    paste0(partition.name, ": ", levels(factor(data[, partition.name])))

  tab

}




