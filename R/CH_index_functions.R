#' Number of cluster selection according to \code{CH} index
#'
#' Wrapper to estimate the best number of clusters according to \code{CH} index.
#'
#' @keywords internal
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param min.nc integer strictly higher than 1: minimum number of clusters.
#' @param max.nc integer (\code{>min.nc}): maximum number of  clusters.
#' @param method Clustering algorithm to use. The only available value for now
#'    is "\code{kmed}" for \code{K-medians} clustering.
#'
#' @return A list containing the selected number of clusters, the CH values and
#'   the best partition.
CH.sel <- function(data, min.nc, max.nc, method){

  alls <- lapply(min.nc:max.nc, CH,
                 data = data, method = method)
  res <- sapply(alls, function(i){
    i[["CH"]]
  }, simplify = TRUE, USE.NAMES = TRUE)
  MAX <- max(res)
  K <- (min.nc:max.nc)[which(res == MAX)[1]]
  names(res) <- paste("k =", min.nc:max.nc)
  return(list(Best.nc = K,
              CritCF.val = res,
              Best.partition = alls[[which(res == MAX)[1]]][["Partition"]]
  )
  )
}

#' \code{CH} index
#'
#' Computes CH index for the selected number of clusters and clustering
#' algorithm.
#'
#' @keywords internal
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param k integer, number of clusters.
#' @param method string, clustering algorithm to use. The only available value
#'   for now is "\code{kmed}" for \code{K-medians} clustering.
#' @param Seed If not \code{null}, passed to \code{set.seed()} before generating
#'   the partition.
#'
#' @return a list, containing the criterion value and the partition.
CH <- function(data, k, method, Seed = 1){
  if (!is.null(Seed)) {set.seed(Seed)}
  if (method == "kmed"){
    kmed <- suppressWarnings(Gmedian::kGmedian(X = data, ncenters = k))
    Classif <- kmed$cluster[, 1]
  }
  Crit <- clusterCrit::intCriteria(traj = as.matrix(data),
                                   part = as.integer(Classif),
                                   crit = "Calinski_Harabasz")

  return(list(CH = Crit[[1]], Partition = Classif))

}
