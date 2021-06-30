#' Number of cluster selection according to CH index
#'
#' Wrapper to estimate the best number of clusters according to CH index
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param min.nc interger strictly higher than 1 : minimum number of clusters.
#' @param max.nc interger (higher than \code{min.nc}) : maximum number of clusters.
#' @param method Clustering algorithm to use.
#'
#' @return A list containing the selected number of clusters, the CH values and
#'   the best partition.
#' @examples  doMIsaul:::CH.sel(iris[, 1:4], 2, 5, "kmed")
CH.sel <- function(data, min.nc, max.nc, method){

  alls <- lapply(min.nc:max.nc, CH,
                 data = data, method = method)
  res <- sapply(alls, function(i){
    i[["CH"]]
  })
  MAX <- max(res)
  K <- (min.nc:max.nc)[which(res == MAX)[1]]
  names(res) <- paste("k =", min.nc:max.nc)
  return(list(Best.nc = K,
              CritCF.val = res,
              Best.partition = alls[[which(res == MAX)[1]]][["Partition"]]
  )
  )
}

#' CH index
#'
#' Computes CH index for the selected number of clusters and clustering
#' algorithm
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param k integer, number of clusters.
#' @param method string, clustering algorithm to use. Only available value for
#'   know is "kmed" for K-medians clsutering.
#'
#' @return a list, containing the criterion value and the partition
#' @examples doMIsaul:::CH(iris[, 1:4], 5, "kmed")
CH <- function(data, k, method){
  set.seed(1)
  if (method == "kmed"){
    kmed <- suppressWarnings(Gmedian::kGmedian(X = data, ncenters = k))
    Classif <- kmed$cluster[, 1]
  }
  Crit <- clusterCrit::intCriteria(traj = as.matrix(data),
                                   part = as.integer(Classif),
                                   crit = "Calinski_Harabasz")

  return(list(CH = Crit[[1]], Partition = Classif))

}
