#' Extract the cluster centers coordinates
#'
#' @keywords internal
#'
#' @param data dataframe (of dimension \code{n * p}).
#' @param Cluster vector (of length \code{n}) containing the cluster
#'   allocations.
#' @param centPos String, indicated whether the center position should be
#'   defined by the mean (use "\code{colMeans}") or the median of the cluster
#'   (use any other string).
#'
#' @return matrix (\code{k * p}) with center coordinates
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

