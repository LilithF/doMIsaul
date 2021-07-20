#' Initiate centers for clustering algorithm
#'
#' @param data Dataset that clustering will be applied on
#' @param N Integer. Number clustering initialization (set of centers) to
#'   generate
#' @param t Numeric between 0 and 1. weight coefficient between only random
#'   centers (t=1) and only centers from clustering (t=0).
#' @param k Vector of size N containing the number of centers for each
#'   initialization.
#' @param algorithms list of algorithm(s) (size \code{N * (1-t)} to generate
#'   centers if t!=1, given as characters. Possible values are "\code{km}" for
#'   'K-means', "\code{kmed}" for 'K-medians',
#'   \code{"hclust.mean", "hclust.med"} for hierarchical clustering with mean
#'    or median position of the center.
#' @param seeds.N (optional) vector of size N containing seeds for each
#'   initialization.
#'
#' @return list of size \code{N} containing coordinates of centers for
#'   clustering initialization.
#' @export
#' @import stats
#' @importFrom dplyr sample_n
#'
#' @examples
#' Cent.init <- initiate_centers(data = iris[, 1:4], N = 10,
#'                               k = sample(c(2:7), 10, replace = TRUE))
initiate_centers <- function(data, N = 1000, t = 1, k,
                             algorithms = NULL, seeds.N = NULL){
  # Generates N sets of centers from data

  n.alea <- t * N
  n.algo <- (1 - t) * N
  if(is.null(seeds.N)){ seeds.N <- runif(N) * 10 ^ 9 }

  if(n.alea>0){

    n.centers <- k[1:n.alea]
    Seeds <- seeds.N[1:n.alea]

    list.centers.alea <- lapply(1:n.alea, function(i){
      set.seed(Seeds[i])
      sample_n(data, size = n.centers[i], replace = FALSE)
    })

  } else {
    list.centers.alea <- list()
  }

  if(n.algo>0){
    n.centers <- k[(1 + n.alea):N]
    Seeds <- seeds.N[(1 + n.alea):N]

    list.centers.algo <- lapply(1:n.algo, function(i){

      set.seed(Seeds[i])
      data.frame(
        switch(algorithms[i],
               kmed = Gmedian::kGmedian(data, ncenters = n.centers[i],
                                        nstart = 1, nstartkmeans = 1,
                                        iter.max = 1)$centers,
               hclust.mean = exctract_center_position(
                 data,
                 cutree(hclust(dist(data)), k = n.centers[i]),
                 "colMeans"),
               hclust.med = exctract_center_position(
                 data,
                 cutree(hclust(dist(data)), k = n.centers[i]),
                 "colMed"),
               km = kmeans(data, centers = n.centers[i])$centers
        ))
    })

  } else {
    list.centers.algo <- list()
  }


  return(c(list.centers.alea, list.centers.algo))
}

