#' Unsupervised learning for incomplete dataset
#'
#' Unsupervised clustering for multiply imputed datasets using \code{MultiCons}
#' consensus (Faucheux et al. 2021 procedure)
#'
#' @usage unsupMI(data, log.data = FALSE, algo = "km", k.crit = "ch",
#'    comb.cons = FALSE, plot.cons = FALSE, return.detail = FALSE,
#'    not.to.use = c("time", "status"))
#'
#' @param data list of the imputed datasets (if present, columns names
#'   \code{time} and \code{status} will be discard)
#' @param log.data logical. Should all columns of the dataset be logged before
#'   applying clustering algorithms?
#' @param algo vector of strings: name of clustering algorithms to use (use
#'   "\code{km}" for \code{k-means}, "\code{kmed}" for \code{K-medians},
#'    "\code{hc"} for \code{hclust} and/or "\code{mclust}" for \code{mclust}).
#' @param k.crit string. Criterion to select the optimal number of clusters (for
#'   each imputed dataset). Use "\code{ch}" for Calinski and Harabasz criterion
#'   (not available for \code{mclust}), "\code{CritCF}" for \code{CriCF} or
#'   \code{bic} for \code{BIC} (\code{mclust} only).
#' @param comb.cons logical. Forced to \code{FALSE} if \code{length(algo)<2}.
#'   Use \code{TRUE} to perform an additional consensus from all partitions
#'   generates, whatever the algorithm.
#' @param plot.cons logical. Use \code{TRUE} to print the MultiCons tree.
#' @param not.to.use vector of strings : names of the columns that should be
#'   discarded for the learning step.
#' @param return.detail logical. Should the detail of imputation specific
#'   partition be returned, in the supplement to the final consensus partition?
#'
#'
#'
#' @return if \code{length(algo)>1} a vector of final cluster ID ; if
#'   \code{length(algo)>1} a data.frame with each column being the final cluster
#'   ID for the corresponding algorithm.
#' @export
#'
#' @examples
#' ## With imputation included TO DO
#'
#' ### With imputation not included
#' ## 1 imputation
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' cancer.imp <- MImpute_surv(cancer, 3)
#' ## 2 learning
#' res <- unsupMI(data = cancer.imp)
#' ## 2.bis learning with several algorithms
#' res.2 <- unsupMI(data = cancer.imp, algo = c("km", "hc"), comb.cons = TRUE,
#'                  plot.cons = TRUE)
unsupMI <- function(data, log.data = FALSE, algo = "km", k.crit = "ch",
                    comb.cons = FALSE, plot.cons = FALSE, return.detail = FALSE,
                    not.to.use = c("time", "status")){

  names.use <- setdiff(colnames(data[[1]]), not.to.use)

  Partition.list <- lapply(1:length(data), function(i){
    partition_generation(data = data[[i]][,  names.use],
                        LOG = log.data, clust.algo = algo, k.crit = k.crit)
  })

  if(length(data) == 1){
    my.part <- Partition.list[[1]]
  } else {
    my.part <- MIclust_mpool(
      list.part = Partition.list,
      plot.MIclust = plot.cons,
      comb.cons = ifelse(length(algo) > 1, comb.cons, FALSE))
  }

  if(return.detail){
    if(length(algo) == 1){
      return(list(Consensus = unlist(unname(my.part)),
                  Detail = Partition.list))
    } else{
      return(list(Consensus = my.part,
                  Detail = Partition.list))
    }

  } else {
    if(length(algo) == 1){
      return(unlist(unname(my.part)))
    } else{
      return(my.part)
    }
  }

}
