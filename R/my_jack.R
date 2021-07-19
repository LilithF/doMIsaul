#' Partition sorting based on Jaccard index
#'
#' Evaluates if all partitions are the same (and thus no consensus algorithm
#' needed) and may pre-select partitions depending on a threshold on the Jaccard
#' index
#'
#' @keywords internal
#'
#' @param data dataframe containing the partitions (factors)
#' @param mcons.JAC.sel minimum required Jaccard index between partitions (0 =
#'   all partitions kept)
#'
#' @return dataframe of partitions with only the partitions satisfying the
#'   conditions kept.
#' @examples
#' partitions <- data.frame(factor(rep(c(1, 2), each = 100)),
#'                          factor(rep(c(1, 2), times = c(120, 80))),
#'                          factor(rep(c(2, 1), each = 100)),
#'                          factor(rep(c(1, 2, 3, 4), each = 50)))
#' dim(doMIsaul:::my_jack(partitions, 0))
#' dim(doMIsaul:::my_jack(partitions, 0.5))
my_jack <- function(data, mcons.JAC.sel = 0){

  x1 <- utils::combn(1:ncol(data), 2)
  jac <- apply(x1, 2, function(j) {
    clusteval::cluster_similarity(data[, j[1]], data[, j[2]],
                                  similarity = "jaccard",
                                  method = "independence")
  })
  if (prod(jac == 1) == 1) {
    return(TRUE)
  } else {
    jac <- jac > mcons.JAC.sel
    if (sum(jac) == ncol(data)) {
      a <- data
    } else {
      if (sum(jac) < 2) {
        a <- FALSE
      } else {
        to.del <- setdiff(as.vector(x1[,!jac]),
                          as.vector(x1[, jac]))
        if (length(to.del) == 0) {
          a <- data
        } else {
          a <- data[,-to.del]
        }
      }
    }
    return(a)
  }
}
