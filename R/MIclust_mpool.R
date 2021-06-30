#' Multicons wrapper for imputed datasets
#'
#' Performs Multicons from a list of partitions
#'
#' @usage MIclust_mpool(list.part, comb.cons, plot.MIclust = FALSE)
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
#' list.partitions <- list(
#'         partition_generation(iris[, 1:4], LOG = FALSE,
#'         clust.algo = c("km", "hc"),
#'         k.crit = "ch"),
#'         partition_generation(iris[, 1:2], LOG = FALSE,
#'         clust.algo = c("km", "hc"),
#'         k.crit = "ch"),
#'         partition_generation(iris[, 2:4], LOG = F,
#'         clust.algo = c("km", "hc"),
#'         k.crit = "ch"))
#'
#' MIclust_mpool(list.partitions, FALSE, FALSE)
MIclust_mpool <- function(list.part, comb.cons, plot.MIclust = FALSE) {
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

          db.part <- my_jack(db.part)

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
              Clust_entry = TRUE,
              returnAll = FALSE
            )$Partitions
          }
        })
      ), 2, factor), stringsAsFactors = TRUE)

  colnames(db) <- algo
  return(db)

}
