#' \code{ggplot} type barplots representing frequencies for each vars.cat by
#'   partition level.
#'
#' @param data The dataset.
#' @param partition.name string. Name of the partition (in data). The partition
#'   variable should be a factor.
#' @param vars.cat vector of strings. variables to plot (categorical only).
#' @param vars.cat.names Optional. Names for displaying the categorical
#'   variables. (given in the same order than \code{vars.cat})
#' @param binary.simplify boolean. Should only the 1st level be kept for binary
#'   variables in \code{vars.cat}?
#' @param unclass.name If applicable, name for the unclassified observations in
#'    the partition.
#' @param include.unclass boolean, should boxplot be displayed for the
#'   unclassified or should they be excluded from the plot.
#'
#' @import ggplot2
#' @return \code{ggplot} object.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- factor(cancer$status)
#' plot_frequency(data = cancer, partition.name = "status",
#'                   vars.cat = c("sex", "ph.ecog"))
#'
#' ## With unclassifieds
#' cancer$status.2 <- as.character(cancer$status)
#' cancer$status.2[sample(1:nrow(cancer), 30)] <- "Unclassif."
#' cancer$status.2 <- factor(cancer$status.2)
#' plot_frequency(data = cancer, partition.name = "status.2",
#'                vars.cat = c("sex", "ph.ecog"),
#'                unclass.name = "Unclassif.", include.unclass = TRUE)
#'
#' ## With unclassifieds (as NA)
#' cancer$status.3 <- cancer$status
#' cancer$status.3[sample(1:nrow(cancer), 30)] <- NA
#' plot_frequency(data = cancer, partition.name = "status.3",
#'                vars.cat = c("sex", "ph.ecog"),
#'                unclass.name = NA, include.unclass = TRUE)
#'
#' plot_frequency(data = cancer, partition.name = "status.3",
#'                vars.cat = c("sex", "ph.ecog", "ph.karno"),
#'                binary.simplify = FALSE,
#'                unclass.name = NA, include.unclass = FALSE)
plot_frequency <- function(data, partition.name,
                           vars.cat, vars.cat.names = NULL,
                           binary.simplify = TRUE,
                           unclass.name = "Unclassified",
                           include.unclass = FALSE){

  if(!include.unclass) {
    data <- data[!data[, partition.name] %in% unclass.name, ]
  } else {
    if(is.na(unclass.name)){
      data[, partition.name] <- factor(data[, partition.name])
      levels(data[, partition.name]) <- c(levels(data[, partition.name]), "NA")
      data[is.na(data[, partition.name]), partition.name] <- "NA"
    }
  }


  nlev <- sapply(data[, vars.cat], function(x){
    nlevels(factor(x, exclude = NULL))
  })

  temp <-  data.frame(
    do.call(cbind,
            lapply(c(vars.cat), function(x){
              uu <- levels(factor(data[, x]))
              data[, x] <- as.character(data[, x])
              data[is.na(data[, x]), x] <- "NA"
              data[, x] <- factor(data[, x],
                                  levels = unique(c(uu, "NA")))[drop = TRUE]

              stats::model.matrix(as.formula(paste("~ 0 +", x)) , data = data)
            })),
    part = data[, partition.name]
    )

  if(binary.simplify){
    to.rm <- sapply(names(nlev)[nlev == 2], function(x){
      paste0(x, levels(factor(data[, x]))[2])
    })

    temp <- temp[, setdiff(colnames(temp), to.rm)]
    nlev[nlev == 2] <- 1
  }

  temp.melt <- reshape2::melt(temp, id.vars = "part",
                              value.name = "value", variable.name = "variable")
  if(is.null(vars.cat.names)) vars.cat.names <- vars.cat
  for(xx in seq_along(vars.cat)){
    temp.melt$variable <- gsub(vars.cat[xx],
                               paste0(vars.cat.names[xx], ": "),
                               temp.melt$variable)
  }

  temp.melt$variable <- factor(temp.melt$variable,
                               levels = rev(unique(temp.melt$variable)))

  temp.melt2 <- dplyr::summarise(
    dplyr::group_by(temp.melt, .data$variable, .data$part),
    freq = 100*sum(.data$value, na.rm = TRUE)/dplyr::n()
  )

  temp.melt2$part <- factor(temp.melt2$part)[drop = TRUE]
  lev.p <- levels(temp.melt2$part)
  rr <- summary(factor(data[, partition.name]))
  temp.melt2$part <- plyr::mapvalues(temp.melt2$part,
                                     lev.p,
                                     paste0(lev.p, "\n(n=", rr,")")
  )

  xlines <- cumsum(rev(nlev)) + .5
  xlines <- xlines[-length(xlines)]

  p <-  ggplot(temp.melt2) +
    geom_bar(aes(x = .data$variable,
                 y = .data$freq,
                 fill = .data$freq), stat = "identity") +
    scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 100),
                        name = "Prevalence in the cluster (%)") +
    coord_flip(ylim = c(0,100)) +
    geom_vline(xintercept = xlines, size = 1) +
    facet_wrap(~part, nrow = 1) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom") +
    xlab("Parameters") +
    ylab("Prevalence")

  p
}

