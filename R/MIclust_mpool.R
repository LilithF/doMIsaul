#' Multicons wrapper for imputed datasets
#'
#' Performs Multicons from a list of partitions
#'
#' @usage MIClust.mpool(list.part, comb.cons, plot.MIclust = F)
#' @param list.part list of partitions with one element of the list corresponds
#'   to the results of one imputed dataset. If more than one algorithm were
#'   tester, each element if the list is a dataframe, as obtained by
#'   partition_generation()
#' @param plot.MIclust should Multicons tree be plotted?
#' @param comb.cons logical. Forced to FALSE if \code{length(algo)<2}. Use
#'   \code{TRUE} to perform an additional consensus from all partitions
#'
#' @return a data frame with ncol = number of algorithms (+1 if comb.cons ==
#'   T), containing the consensus partitions
#' @examples
#' list.partitions <- partition_generation(iris[, 1:4], LOG = F, clust.algo = c("km", "hc"), k.crit = "ch")
#' MIclust_mpool(list.partitions, F, F)
MIclust_mpool <- function(list.part, comb.cons, plot.MIclust = F) {
  algo <- colnames(list.part[[1]])

  if (comb.cons & length(algo) > 1) {
    algo <- c(algo, "all")
  }
  mi.m <- length(list.part)

  db <- data.frame(apply(do.call(cbind,
                                 lapply(algo, function(i) {
                                   if (i %in% "all") {
                                     ind <- 1:ncol(list.part[[1]])
                                   } else {
                                     ind <- i
                                   }

                                   db.part <- data.frame(apply(do.call(cbind,
                                                                       lapply(1:mi.m, function(m) {
                                                                         list.part[[m]][, ind]
                                                                       })), 2, factor), stringsAsFactors = T)

                                   colnames(db.part) <- paste0("X", 1:ncol(db.part))

                                   db.part <- my.jack(db.part)

                                   if (is.logical(db.part)) {
                                     if (db.part) {
                                       t1 <- list.part[[1]][, ind[1]]
                                     } else {
                                       rep(NA, list.part[[1]][, ind[1]])
                                     }
                                   } else {
                                     t1 <- MultiCons(
                                       DB = db.part,
                                       Plot = plot.MIclust,
                                       Clust_entry = T,
                                       returnAll = F
                                     )$Partitions
                                   }
                                 })), 2, factor), stringsAsFactors = T)

  colnames(db) <- algo
  return(db)

}
