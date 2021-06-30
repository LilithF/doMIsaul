#' Number of cluster selection according to CritCF index
#'
#' Wrapper to estimate the best number of clusters according to CritCF index
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param min.nc interger strictly higher than 1 : minimum number of clusters.
#' @param max.nc interger (higher than min.nc) : maximum number of clusters.
#' @param method clustering algorithm to use.
#' @param distance distance between the observations (either euclidean or manhattan).
#'
#' @return A list containing the selected number of clusters, the CritCF values
#'   and the best partition.
#' @examples doMIsaul:::CritCF.sel(iris[, 1:4], 2, 9, "kmeans", "euclidean")
CritCF.sel <- function(data, min.nc, max.nc, method, distance){

  alls <- lapply(min.nc:max.nc, CritCF,
                 data = data, method = method, distance = distance)
  res <- sapply(alls, function(i){
    i[["CritCF"]]
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


#' CritCF index
#'
#' Computes CritCF index for the selected number of clusters and clustering
#' algorithm
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param k integer, number of clusters.
#' @param method string, clustering algorithm to use. Available values are
#'  kmeans, hc (for hclust) or mclust.
#' @param distance distance between the observations (either euclidean or manhattan).
#'
#' @return a list, containing the criterion value and the partition
#' @examples doMIsaul:::CritCF(iris[, 1:4], 5, "hc", "euclidean")
CritCF <- function(data, k, method, distance){

  if("mclust" %in% method) {
    requireNamespace("mclust", quietly = F)
  }

  p <- ncol(data)
  power <- switch(distance, euclidean = 2, manhattan = 1)
  set.seed(1)
  if (method == "kmeans"){
    km <- stats::kmeans(data, centers = k)
    Classif <- km$cluster
    Centres <- km$centers
    Card <- km$size
  } else {
    Classif <- switch(
      method,
      hc = stats::cutree(stats::hclust(dist(data, method = distance)), k = k),
      mclust = mclust::Mclust(data, G = k, verbose = F)$classification
    )
    Centres <- data.frame(t(sapply(1:k, function(i){
      colMeans(data[Classif == i, ])
    }
    )))
    Card <- as.vector(table(Classif))
  }

  Data.Center <- colMeans(data)
  Centres.2 <- (abs(Centres - matrix(
    rep(Data.Center, k), byrow = T, nrow = k)
  )) ^ power
  B <- sum(Card * ( (rowSums(Centres.2)) ^ (1 / power)))
  data.2 <- (abs(data - Centres[Classif, ])) ^ power
  W <- sum( (rowSums(data.2)) ^ (1 / power))

  Crit <- ( (2 * p / (2 * p + 1)) * (1 / (1 + W / B))) ^
    ( (log(k + 1, base = 2) + 1) / (log(p + 1, base = 2) + 1))

  return(list(CritCF = Crit, Partition = Classif))

}

