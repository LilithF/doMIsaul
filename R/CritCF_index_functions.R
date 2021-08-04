#' Number of cluster selection according to \code{CritCF} index
#'
#' Wrapper to estimate the best number of clusters according to \code{CritCF}
#'  index
#'
#' @keywords internal
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param min.nc integer strictly higher than 1: minimum number of clusters.
#' @param max.nc integer (\code{>min.nc}): maximum number of clusters.
#' @param method string, clustering algorithm to use. Available values are
#'  "\code{kmeans}", "\code{hc}" (for \code{hclust()}) or "\code{mclust}".
#' @param distance string, distance between the observations (either "euclidean"
#'    or "manhattan").
#'
#' @return A list containing the selected number of clusters, the \code{CritCF}
#'   values and the best partition.
CritCF.sel <- function(data, min.nc, max.nc, method, distance){

  alls <- lapply(min.nc:max.nc, CritCF,
                 data = data, method = method, distance = distance)
  res <- sapply(alls, function(i){
    i[["CritCF"]]
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


#' \code{CritCF} index
#'
#' Computes \code{CritCF} index for the selected number of clusters and
#'   clustering algorithm.
#'
#' @keywords internal
#'
#' @param data dataframe for which the number of cluster should be estimated.
#' @param k integer, number of clusters.
#' @param method string, clustering algorithm to use. Available values are
#'  "\code{kmeans}", "\code{hc}" (for \code{hclust()}) or "\code{mclust}".
#' @param distance string, distance between the observations (either "euclidean"
#'    or "manhattan").
#' @param Seed If not \code{null}, passed to \code{set.seed()} before generating
#'   the partition.
#'
#' @return a list, containing the criterion value and the partition
CritCF <- function(data, k, method, distance, Seed = 1){

  if("mclust" %in% method) {
    requireNamespace("mclust", quietly = FALSE)
  }

  p <- ncol(data)
  power <- switch(distance, euclidean = 2, manhattan = 1)
  if (!is.null(Seed)) {set.seed(Seed)}
  if (method == "kmeans"){
    km <- stats::kmeans(data, centers = k)
    Classif <- km$cluster
    Centres <- km$centers
    Card <- km$size
  } else {
    Classif <- switch(
      method,
      hc = stats::cutree(stats::hclust(dist(data, method = distance)), k = k),
      mclust = mclust::Mclust(data, G = k, verbose = FALSE)$classification
    )
    Centres <- data.frame(t(sapply(1:k, function(i){
      colMeans(data[Classif == i, ])
    }, simplify = TRUE, USE.NAMES = TRUE
    )))
    Card <- as.vector(table(Classif))
  }

  Data.Center <- colMeans(data)
  Centres.2 <- (abs(Centres - matrix(
    rep(Data.Center, k), byrow = TRUE, nrow = k)
  )) ^ power
  B <- sum(Card * ( (rowSums(Centres.2)) ^ (1 / power)))
  data.2 <- (abs(data - Centres[Classif, ])) ^ power
  W <- sum( (rowSums(data.2)) ^ (1 / power))

  Crit <- ( (2 * p / (2 * p + 1)) * (1 / (1 + W / B))) ^
    ( (log(k + 1, base = 2) + 1) / (log(p + 1, base = 2) + 1))

  return(list(CritCF = Crit, Partition = Classif))

}

