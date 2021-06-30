#' objective clustering cost
#'
#' Auxiliary function to calculate a cost function for clustering. Modified
#' version of Cost_func from Khorshidi et al., 2019
#'
#' @param Alloc_Dist list with Distance to cluster centers and allocation
#'   matrix, as obtained with Allocation_Distance()
#'
#' @return matrix
#' @examples
#' allocation <- doMIsaul:::Allocation_Distance(iris[, 1:4], iris[c(1,5, 10), 1:4])
#' doMIsaul:::objective_clustering(allocation)
objective_clustering <- function(Alloc_Dist){
  tmp <- Alloc_Dist$Allocation * Alloc_Dist$Distance
  A <-
    sum(colSums(tmp) / apply(
      tmp,
      2,
      FUN = function(x) {
        length(x[x != 0])
      }
    ), na.rm = T)
  return(A)
}
