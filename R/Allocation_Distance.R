#' Allocation Distance
#'
# Auxiliary function to allocate data points into clusters from
#'  Khorshidi et al., 2019
#'
#' @param Z dataset
#' @param Xr Center coordinates
#'
#' @return List with distances to center and allocation matrix
#' @examples doMIsaul:::Allocation_Distance(iris[, 1:4], iris[c(1,5, 10), 1:4])
Allocation_Distance <- function(Z, Xr){

  Dis <- matrix(0, nrow = nrow(Z), ncol = nrow(Xr))
  Amat <-  matrix(0, nrow = nrow(Z), ncol = nrow(Xr))
  for (j in 1:nrow(Xr)) {
    Dis[, j] <-
      matrix(apply(Z, 1, FUN = function(x){
        sum(abs(x - Xr[j, ]))
      }), , 1)
  }

  # for each point find the cluster with the minimum distance
  min.dist <- Dis == apply(Dis, 1, min)
  for (n in 1:nrow(min.dist)) {
    if (length(which(min.dist[n, ])) > 1) {
      min.dist[n, sample(which(min.dist[n, ]), 1)] <- FALSE
    }
  }
  Amat[min.dist] <- 1 # assign each point to the cluster with
                      # the highest probability
  Amat[!min.dist] <- 0 # remove points from clusters with lower probabilities
  return(list(Distance = Dis, Allocation = Amat))
}
