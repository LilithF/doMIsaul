#' Extract the cluster centers coordinates
#'
#' @keywords internal
#'
#' @param data dataframe (\code{n * p}).
#' @param Cluster vector (\code{n}) of cluster allocation.
#' @param centPos Cluster: center is defined by mean (\code{colMeans()}) or
#'   median of the cluster.
#'
#' @return matrix (\code{k * p}) with center coordinates
#' @examples
#' doMIsaul:::exctract_center_position(iris[, 1:4],
#'                                     as.numeric(iris[, 5]),
#'                                     "colMeans")
exctract_center_position <- function (data, Cluster, centPos){
  # Extracts cluster centers from a clustering result

  NCluster <- length(levels(as.factor(Cluster)))
  if (class(data) == "numeric") data <- as.matrix(data)
  centroid <- matrix(NA, nrow = NCluster, ncol = ncol(data))
  if (centPos == "colMeans") {
    for (i in 1:NCluster) centroid[i, ] <-
        apply(as.matrix(data[Cluster == i, ]), 2, mean)
  } else {
    for (i in 1:NCluster) centroid[i, ] <-
        apply(as.matrix(data[Cluster == i, ]), 2, median)
  }
  centroid
}

