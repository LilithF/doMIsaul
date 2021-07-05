#' ggplot type boxplots for each vars.cont by partition level.
#'
#' @param data The dataset.
#' @param partition.name string. Name of the partition (in data). The partition
#'   variable should be a factor.
#' @param vars.cont vector of strings. variables to plot (continuous only).
#' @param vars.cont.names Optional. Names for displaying the continuous
#'   variables. (given in the same order than \code{vars.cont})
#' @param unclass.name If applicable, name for the unclassified observations in
#'    the partition.
#' @param include.unclass boolean, should boxplot be displayed for the
#'   unclassified or should they be excluded from the plot.
#'
#' @import ggplot2
#' @return ggplot object.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- factor(cancer$status)
#' plot_boxplot(data = cancer, partition.name = "status",
#'                   vars.cont = c("age", "meal.cal", "wt.loss"))
#'
#' ## With unclassifieds
#' cancer$status.2 <- as.character(cancer$status)
#' cancer$status.2[sample(1:nrow(cancer), 30)] <- "Unclassif."
#' cancer$status.2 <- factor(cancer$status.2)
#' plot_boxplot(data = cancer, partition.name = "status.2",
#'                   vars.cont = c("age", "meal.cal", "wt.loss"),
#'                   unclass.name = "Unclassif.", include.unclass = TRUE)
#'
#' ## With unclassifieds (as NA)
#' cancer$status.3 <- cancer$status
#' cancer$status.3[sample(1:nrow(cancer), 30)] <- NA
#' plot_boxplot(data = cancer, partition.name = "status.3",
#'                   vars.cont = c("age", "meal.cal", "wt.loss"),
#'                   unclass.name = NA, include.unclass = TRUE)
plot_boxplot <- function(data, partition.name,
                         vars.cont, vars.cont.names = NULL,
                         unclass.name = "Unclassified",
                         include.unclass = FALSE){

  temp <- reshape2::melt(data, measure.vars = vars.cont)
  if(!is.null(vars.cont.names)){
    temp$variable <- plyr::mapvalues(temp$variable, vars.cont, vars.cont.names)
  }

  if(!include.unclass) {
    temp <- temp[!temp[, partition.name] %in% unclass.name, ]
  } else {
    if(is.na(unclass.name)){
      temp[, partition.name] <- factor(temp[, partition.name])
      levels(temp[, partition.name]) <- c(levels(temp[, partition.name]), "NA")
      temp[is.na(temp[, partition.name]), partition.name] <- "NA"
    }
  }

  p <- ggplot(temp, aes(x = eval(parse(text = partition.name)), y = value)) +
    geom_boxplot(aes(fill = eval(parse(text = partition.name)))) +
    facet_wrap(~variable, scales = "free_y") +
    scale_fill_discrete(name = partition.name) +
    xlab(partition.name)

  p
}
