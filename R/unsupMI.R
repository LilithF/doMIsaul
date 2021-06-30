#' Unsupervised learning for incomplete dataset
#'
#' Unsupervised clustering for multiply imputed datasets unsing MultiCons
#' consensus (Faucheux et al. 2020 procedure)
#'
#' @usage unsupMI(data, log.data = F, algo = "km", k.crit =
#'   "ch", comb.cons = F, plot.cons = F, return.detail = F, not.to.use =
#'    c("time", "status"))
#'
#' @param data list of the imupted datasets (if present, columns names "time"
#'   and "status" will be discared)
#' @param log.data logical. Should all columns of the dataset be logged before
#'   applying clustering algorithms?
#' @param algo vector of strings: name of clsutering algorithms to use (use "km"
#'   for k-means, "kmed" for K-medians, "hc" for hclust and/or "mclust" for
#'   mclust).
#' @param k.crit string. Criterion to select the optimal number of clusters (for
#'   each imputed dataset). Use "ch" for Calinski and Harabasz criterion (not
#'   available for mclust), "CritCF" for CriCF or bic for BIC (mclust only).
#' @param comb.cons logical. Forced to FALSE if \code{length(algo)<2}. Use
#'   \code{TRUE} to perform an additional consensus from all partitions
#'   generates, whaterver the algorithm.
#' @param plot.cons logical. Use \code{TRUE} to print the MultiCons tree.
#' @param not.to.use vector of strings : names of the columns that should be
#'   discraded for the learning step.
#' @param return.detail logical. Should the detail of imputation specific
#'   partition be returned, in the supplument to the final consensus partition?
#'
#'
#'
#' @return if \code{length(algo)>1} a vector of final cluster ID ; if
#'   \code{length(algo)>1} a data.frame with each column being the final cluster
#'   ID for the corresponding algorithm.
#' @export
#'
#' @examples
#' # unsupMI()
unsupMI <- function(data, log.data = F, algo = "km", k.crit = "ch", comb.cons = F,
                    plot.cons = F, return.detail = F, not.to.use = c("time", "status")){

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
      comb.cons = ifelse(length(algo) > 1, comb.cons, F))
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
