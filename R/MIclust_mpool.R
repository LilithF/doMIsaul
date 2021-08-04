#' \code{MultiCons} wrapper for imputed datasets
#'
#' Performs \code{MultiCons()} from a list of partitions
#'
#' @keywords internal
#'
#'
#' @param list.part list of partitions, where one element of the list
#'   corresponds to the clustering results for one imputed dataset. If more than
#'   one clustering algorithm were used, each element if the list is a
#'   dataframe, as obtained by \code{partition_generation()}.
#' @param comb.cons Boolean, use \code{TRUE} to perform an additional consensus
#'   from all partitions (ie. one consensus per clustering algorithm used, plus
#'   one consensus of all partitions: mixing all clustering algorithms used).
#'   This parameter is forced to \code{FALSE} if \code{length(algo)<2}.
#' @param mcons.JAC.sel Numeric (in (0,1)) passed to internal function
#'   \code{my_jack()}. Minimum \code{Jaccard index} value between partitions to
#'   keep them for the consensus.
#' @param plot.MIclust Boolean, should \code{MultiCons()} tree be plotted?
#'
#' @return a data frame with \code{ncol()} = number of algorithms (+1 if
#'   \code{comb.cons == T}), containing the consensus partitions.
MIclust_mpool <- function(list.part, comb.cons,
                          mcons.JAC.sel = 0, plot.MIclust = FALSE) {
  algo <- colnames(list.part[[1]])

  if (comb.cons & length(algo) > 1) {
    algo <- c(algo, "all")
  }
  mi.m <- length(list.part)

  db <- data.frame(
    apply(
      do.call(
        cbind,
        lapply(algo, function(i) {
          if (i %in% "all") {
            ind <- 1:ncol(list.part[[1]])
          } else {
            ind <- i
          }

          db.part <- data.frame(
            apply(do.call(cbind,
                          lapply(1:mi.m, function(m) {
                            list.part[[m]][, ind]
                          })), 2, factor), stringsAsFactors = TRUE)

          colnames(db.part) <- paste0("X", 1:ncol(db.part))

          db.part <- my_jack(db.part, mcons.JAC.sel = mcons.JAC.sel)

          if (is.logical(db.part)) {
            if (db.part) {
              t1 <- list.part[[1]][, ind[1]]
            } else {
              rep(NA, length(list.part[[1]][, ind[1]]))
            }
          } else {
            t1 <- MultiCons(
              DB = db.part,
              Plot = plot.MIclust,
              Clust_entry = TRUE,
              returnAll = FALSE
            )$Partitions
          }
        })
      ), 2, factor), stringsAsFactors = TRUE)

  colnames(db) <- algo
  return(db)

}
