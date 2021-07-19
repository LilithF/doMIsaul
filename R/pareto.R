#' Pareto optimization
#'
#' Find Pareto optimal and Pareto non optimal solutions
#'
#' @keywords internal
#'
#' @param bi.objective dataframe or matrix containing the objective values for
#'   each case
#' @param obj.names name of the two objectives (should be in
#'   \code{colnames(bi.objetive)} )
#'
#' @return the entry bi.objective with an additional column "Dominated" with 1
#'   for non optimal solutions and 0 for optimal.
#' @examples
#' tab <- data.frame(A = rnorm(100, 10, 3), B = runif(100, 3,4))
#' doMIsaul:::pareto(tab, c("A", "B"))
pareto <- function(bi.objective, obj.names = c("Cluster", "Regression")){
  for (r in 1:nrow(bi.objective)) {
    Dominated <- 0
    set <- setdiff(1:nrow(bi.objective), r)
    i <- 1
    exit <- FALSE
    while (!exit) {
      rr <- set[i]
      if (bi.objective[r, obj.names[1]] > bi.objective[rr, obj.names[1]] &
          bi.objective[r, obj.names[2]] > bi.objective[rr, obj.names[2]] ) {
        Dominated <- Dominated + 1
      }
      i <- i + 1
      exit <- Dominated > 0 | (all.equal(rr, max(set)) == TRUE)
    }

    bi.objective[r, 'Dominated'] <- Dominated
  }

  return(bi.objective)
}

