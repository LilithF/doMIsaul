#' Partition sorting based on Jaccard index
#'
#' Evaluates if all partitions are the same (and thus no consensus algorithm
#' needed) and may pre-select partitions depending on a threshold on the Jaccard
#' index
#'
#' @keywords internal
#'
#' @param data dataframe containing the partitions (factors). Specifically, each
#'  column must be a partition and must be a factor.
#' @param mcons.JAC.sel Numeric (in (0, 1)), minimum \code{Jaccard index} value
#'   between partitions to keep them (0 = all partitions kept).
#'
#' @return dataframe of partitions with only the partitions satisfying the
#'   conditions kept.
my_jack <- function(data, mcons.JAC.sel = 0){

  x1 <- utils::combn(1:ncol(data), 2)
  jac <- apply(x1, 2, function(j) {
    clusterCrit::extCriteria(as.integer(data[, j[1]]),
                             as.integer(data[, j[2]]),
                             crit = "Jaccard")[[1]]
  })
  if (prod(jac == 1) == 1) {
    return(TRUE)
  } else {
    jac <- jac > mcons.JAC.sel
    if (sum(jac) == ncol(x1)) {
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
