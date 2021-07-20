#' Unsupervised partition with K selection
#'
#' Generates a partition using \code{clust.algo} algorithm, with \code{k.crit}
#'  for selecting the number of clusters
#'
#' @param data dataframe to cluster
#' @param LOG logical. Should all columns of the dataset be logged before
#'   applying clustering algorithms?
#' @param clust.algo vector of strings: name of clustering algorithms to use
#'   (use \code{"km"} for k-means, \code{"kmed"} for K-medians, \code{"hc"} for
#'    \code{hclust()} and/or \code{"mclust"} for \code{mclust()}).
#' @param k.crit string. Criterion to select the optimal number of clusters (for
#'   each imputed dataset). Use \code{"ch"} for Calinski and Harabasz criterion
#'   (not available for "\code{mclust}"), \code{"CritCF"} for \code{CritCF} or
#'   \code{bic} for BIC ("\code{mclust}" only).
#'
#' @return a dataframe with one column for each algorithm in \code{clust.algo},
#'   containing the cluster IDs.
#' @export
#' @import NbClust
#'
#' @examples
#' partition_generation(iris[, 1:4], LOG = FALSE,
#'                      clust.algo = c("km", "hc"), k.crit = "ch")
partition_generation <- function(data, LOG, clust.algo, k.crit){

  if("mclust" %in% clust.algo) {
    requireNamespace("mclust", quietly = FALSE)
  }


  if (LOG) {
    data.tmp <- log(data)
  } else {
    data.tmp <- data
  }


  Parts <- data.frame(apply(
    do.call(cbind, lapply(clust.algo, function(ALG){
      switch(
        k.crit,
        ch = switch(ALG,
                    km = NbClust(data.tmp, distance = "euclidean",
                                 min.nc = 2, max.nc = 7,
                                 method = "kmeans",
                                 index = "ch")$Best.partition,
                    hc = NbClust(data.tmp, distance = "euclidean",
                                 min.nc = 1, max.nc = 7,
                                 method = "complete",
                                 index = "ch")$Best.partition,
                    kmed = CH.sel(data = data.tmp, min.nc = 2,
                                  max.nc = 7, method = "kmed")$Best.partition,
                    mclust = mclust::Mclust(data.tmp,
                                            G = 1:7,
                                            verbose = FALSE)$classification),
        CritCF = switch(ALG,
                        km = CritCF.sel(data = data.tmp, min.nc = 2,
                                        max.nc = 7, method = "kmeans",
                                        distance = "euclidean")$Best.partition,
                        hc = CritCF.sel(data = data.tmp, min.nc = 2,
                                        max.nc = 7, method = "hc",
                                        distance = "euclidean")$Best.partition,
                        mclust = CritCF.sel(data = data.tmp, min.nc = 2,
                                            max.nc = 7, distance = "euclidean",
                                            method = "mclust")$Best.partition),
        bic = switch(ALG,
                     mclust = mclust::Mclust(data.tmp,
                                             G = 1:7,
                                             verbose = FALSE)$classification)
      )
    }
    )), 2, factor))

  colnames(Parts) <- clust.algo

  return(Parts)

}
