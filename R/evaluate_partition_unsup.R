#' Comparison of an unsupervised obtained partition to a reference partition.
#'
#' Compares partitions on number of cluster, ARI and percentage of unclassified
#' observations.
#'
#' @param partition vector (factor): the partition to evaluate.
#' @param partition.ref reference partition 1 (ex partition on complete data or
#' true partition if known).
#' @param is.missing boolean vector identifying observations with missing data
#'   (coded \code{TRUE}), from those without (coded \code{FALSE}).
#' @param is.cens the incomplete dataframe with NA for missing and
#'   left-censored data (or the complete datasets if all data were observed).
#'
#' @return A list containing the following elements : \code{Nbclust}: number of
#'   clusters of the partition; \code{ARI}: ARI value on cases classified by
#'   both partitions ;  \code{ARI.cc}: ARI value on cases complete AND
#'   classified by both partitions ; \code{ARI.nona}; ARI on cases with no
#'   missing data AND classified by both partitions; \code{ARI.nocens}: ARI on
#'   cases with no censored data AND classified by both partitions ;
#'   \code{Per.Unclass}: Percentage of observations unclassified in the
#'   partition ; \code{Per.Unclass.cc}: Among complete cases, percentage of
#'   observations unclassified in the partition ; \code{Per.Unclass.na}:
#'   Among cases with missing data, percentage of observations unclassified in
#'    the partition ; \code{Per.Unclass.cens}: Among cases with censored data,
#'    percentage of observations unclassified in the partition ;
#'    \code{Per.Unclass.ic}: Among incomplete cases, percentage of observations
#'    unclassified in the partition
#' @export
#' @importFrom aricode ARI
#'
#' @examples
#' res <- evaluate_partition_unsup(
#'   partition = factor(rep(c(1,2,3), each = 50)),
#'   partition.ref = factor(rep(c(1,2,3), times = c(100, 25, 25))))
#'
#' ## With missing data
#' res2 <- evaluate_partition_unsup(
#'   partition = factor(rep(c(1,2,3), each = 50)),
#'   partition.ref = factor(rep(c(1,2,3), times = c(100, 25, 25))),
#'   is.missing = sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(.2,.8)))
#'
#' ## With missing and censored data
#'   missing.indicator <- sample(c(TRUE, FALSE), 150,
#'    replace = TRUE, prob = c(.2,.8))
#'   Censor.indicator <- data.frame(
#'    X1 = runif(150, 1, 5),
#'    X2 = runif(150, 6, 8),
#'    X3 = runif(150, 3, 9))
#'   Censor.indicator$X1[missing.indicator] <- NA
#'   Censor.indicator$X1[
#'   sample(c(TRUE, FALSE), 150,replace = TRUE, prob = c(.1,.9))] <- NA
#'   Censor.indicator$X2[
#'   sample(c(TRUE, FALSE), 150,replace = TRUE, prob = c(.3,.7))] <- NA
#'   Censor.indicator$X3[
#'   sample(c(TRUE, FALSE), 150,replace = TRUE, prob = c(.05,.95))] <- NA
#' res3 <- evaluate_partition_unsup(
#'   partition = factor(rep(c(1,2,3), each = 50)),
#'   partition.ref = factor(rep(c(1,2,3), times = c(100, 25, 25))),
#'   is.missing = missing.indicator,
#'   is.cens = Censor.indicator)
#'
#' ## With missing and censored data and unclassifed observations
#' res4 <- evaluate_partition_unsup(
#'   partition = factor(rep(c(1,2, NA,3), times = c(50, 40, 20, 40))),
#'   partition.ref = factor(rep(c(1,2,3), times = c(100, 25, 25))),
#'   is.missing = missing.indicator,
#'   is.cens = Censor.indicator)
evaluate_partition_unsup <-
  function(partition, partition.ref, is.missing = NULL, is.cens = NULL){

    if(!is.null(is.missing)){
      miss.ind <- is.missing
    } else {
      miss.ind <- rep(FALSE, length(partition))
    }

    if(!is.null(is.cens)){
      CC.data <- complete.cases(is.cens)
      cens.ind <- !CC.data & !is.missing

    } else {
      CC.data <- rep(TRUE, length(partition))
      cens.ind <- rep(FALSE, length(partition))
    }

    part.class <- complete.cases(partition)
    ref.class <- complete.cases(partition.ref)
    CC <-  part.class & ref.class

    if (length(partition[CC]) > 0) {
      ARI <- ARI(partition[CC], partition.ref[CC])
      ARI.cc <- ARI(partition[CC.data & CC], partition.ref[CC.data & CC])
      ARI.nona <- ARI(partition[!miss.ind & CC], partition.ref[!miss.ind & CC])
      ARI.nocens <- ARI(partition[!cens.ind & CC],
                        partition.ref[!cens.ind & CC])
      Nbclust <- length(levels(factor(partition)))
    } else {
      ARI <- ARI.cc <- ARI.nona <- ARI.nocens <- Nbclust <- NA
    }

    Per.Unclass <- sum(!part.class) / length(part.class)
    Per.Unclass.cc <- sum(!part.class & CC.data) / sum(CC.data)
    Per.Unclass.na <- sum(!part.class & miss.ind) / sum(miss.ind)
    Per.Unclass.cens <- sum(!part.class & cens.ind) / sum(cens.ind)
    Per.Unclass.ic <- sum(!part.class & !CC.data ) / sum(!CC.data)


    return(list(Nbclust = Nbclust,
                ARI = ARI,
                ARI.cc = ARI.cc,
                ARI.nona = ARI.nona,
                ARI.nocens = ARI.nocens,
                Per.Unclass = Per.Unclass,
                Per.Unclass.cc = Per.Unclass.cc,
                Per.Unclass.na = Per.Unclass.na,
                Per.Unclass.cens = Per.Unclass.cens,
                Per.Unclass.ic = Per.Unclass.ic
    ))

  }
