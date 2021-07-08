#' Remove small clusters (i.e. unclassified observations for which no consensus
#'  was obtained)
#'
#' @param partition the partition to clean (vector).
#' @param min.cluster.size Minimum cluster size (i.e., smaller clusters will be
#'   discarded)
#' @param level.order optional. If you supply a variable the  cluster levels
#'  will be ordinated according to the mean values for the variable
#' @param Unclassified string for the label of the unclassified observations.
#'   defaults value is \code{NA}.
#'
#' @return The cleaned up partition (factor).
#' @export
#' @importFrom plyr mapvalues
#'
#' @examples
#' part <- factor(kmeans(iris[, 1:4], 8)$cluster)
#' summary(part)
#' part.clean <- cleanUp_partition(part, Unclassified = "Unclassified")
cleanUp_partition <- function(partition, min.cluster.size = 10,
                              level.order = NULL,
                              Unclassified = c(NA, "Unclassified")) {
  ret <- as.character(partition)
  smr <- summary(factor(partition), maxsum = length(partition))
  to.del <- names(smr)[smr < min.cluster.size]
  if(length(to.del) > 0) {
    ret[ret %in% to.del] <- NA
  }

  ret <- factor(ret)

  if(!is.null(level.order)){
    or.lev <- sapply(levels(ret), function(x){
      mean(level.order[ret %in% x], na.rm = TRUE)

    })


  } else {
    or.lev <- seq_along(levels(ret)[drop = TRUE])
  }

  ret <- factor(as.character(
    mapvalues(ret, levels(ret), (1:nlevels(ret))[rank(or.lev)])
  ))

  if(!is.na(Unclassified[1])) {
    levels(ret) <- c(levels(ret), Unclassified[1])
    ret[is.na(ret)] <- Unclassified[1]
  }


  return(ret[drop = TRUE])
}
