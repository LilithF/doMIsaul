#' objective clustering cost
#'
#' Auxiliary function to calculate a cost function for clustering. Modified
#' version of \code{Cost_func()} from \code{Khorshidi et al., 2019}.
#'
#' @keywords internal
#'
#' @param Alloc_Dist list with Distance to cluster centers and allocation
#'   matrix, as obtained with \code{Allocation_Distance()}.
#'
#' @return matrix
objective_clustering <- function(Alloc_Dist){
  tmp <- Alloc_Dist$Allocation * Alloc_Dist$Distance
  A <-
    sum(colSums(tmp) / apply(
      tmp,
      2,
      FUN = function(x) {
        length(x[x != 0])
      }
    ), na.rm = TRUE)
  return(A)
}
