#' Unsupervised learning for incomplete dataset
#'
#' Unsupervised clustering for multiply imputed datasets using
#'   \code{MultiCons()} consensus (\code{Faucheux et al. 2021} procedure)
#'
#'
#'
#' @param Impute Default is \code{FALSE} to indicate that the user performed
#'   the imputation and provides the imputed data. Otherwise string
#'   ("\code{MImpute}", "\code{MImpute_surv}" or "\code{MImpute_lcens}") to
#'   perform the imputation within the call using the \code{MImpute()},
#'   \code{MImpute_surv()} or \code{MImpute_lcens()} function.
#' @param Impute.m Used only if Impute is not \code{FALSE} ; number of
#'   imputations to perform
#' @param data Data, in the form of a list of data.frame(s). The list should be
#'   one length 1 if data are complete or if Impute is not \code{FALSE}, it
#'   should be a list of imputed dataframes if data are incomplete and imputed.
#'   If some columns are in \code{not.to.use}, they will be discarded for the
#'   clustering.
#' @param log.data logical. Should all columns of the dataset be logged before
#'   applying clustering algorithms?
#' @param algo vector of strings: name of clustering algorithms to use (use
#'   "\code{km}" for \code{k-means}, "\code{kmed}" for \code{K-medians},
#'   "\code{hc}" for \code{hclust()} and/or "\code{mclust}" for
#'    \code{mclust()}).
#' @param k.crit string. Criterion to select the optimal number of clusters (for
#'   each imputed dataset). Use "\code{ch}" for Calinski and Harabasz criterion
#'   (not available for \code{mclust}), "\code{CritCF}" for \code{CritCF} or
#'   "\code{bic}" for \code{BIC} (\code{mclust} only).
#' @param comb.cons logical. Forced to \code{FALSE} if \code{length(algo)<2}.
#'   Use \code{TRUE} to perform an additional consensus from all partitions
#'   generates, whatever the algorithm.
#' @param plot.cons logical. Use \code{TRUE} to print the MultiCons tree. Note
#'   that if all partitions are identical across the imputation no consensus
#'   will be performed and therefore not plot will be obtained even if
#'   \code{plot.cons = TRUE}.
#' @param not.to.use vector of strings : names of the columns that should be
#'   discarded for the learning step.
#' @param return.detail logical. Should the detail of imputation specific
#'   partition and the imputed data be returned, in the supplement to the final
#'   consensus partition?
#' @param cens.data.lod passed to \code{MImpute_lcens()} if
#'   \code{Impute == MImpute_lcens}
#' @param cens.standards passed to \code{MImpute_lcens()} if
#'   \code{Impute == MImpute_lcens}
#' @param cens.mice.log passed to \code{MImpute_lcens()} if
#'   \code{Impute == MImpute_lcens}
#' @param cleanup.partition should the partition be trimmed of small clusters.
#'   (The consensus may generate small clusters of observations for which there
#'     is no consensus on the cluster assignation)
#' @param min.cluster.size if \code{cleanup.partition == TRUE}: Minimum
#'  cluster size (i.e., smaller clusters will be discarded)
#' @param level.order if \code{cleanup.partition == TRUE}: optional. If you
#' supply a variable the  cluster levels will be ordinated according to the
#' mean values for the variable
#' @param Unclassified if \code{cleanup.partition == TRUE} string for the label
#' of the unclassified observations. defaults value is \code{NA}.
#'
#' @return if \code{length(algo)>1} a vector of final cluster ID ; if
#'   \code{length(algo)>1} a data.frame with each column being the final cluster
#'   ID for the corresponding algorithm. Or if \code{return.detail == TRUE}, a
#'   list containing \code{Consensus} : the final cluster ID (or data.frame),
#'   \code{Detail}: the clusters obtained for each imputed dataset,
#'   \code{Imputed.data} a list containing the imputed datasets.
#' @export
#'
#' @examples
#' ### With imputation included
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' res.0 <- unsupMI(data = list(cancer), Impute = "MImpute_surv",
#'                  cleanup.partition = FALSE)
#'
#' ### With imputation not included
#' ## 1 imputation
#' cancer.imp <- MImpute_surv(cancer, 3)
#' ## 2 learning
#' res <- unsupMI(data = cancer.imp, cleanup.partition = FALSE)
#' summary(factor(res))
#' res.1 <- unsupMI(data = cancer.imp)
#' summary(factor(res.1))
#'
#' ## 2.bis learning with several algorithms
#' res.2 <- unsupMI(data = cancer.imp, algo = c("km", "hc"), comb.cons = TRUE,
#'                  plot.cons = TRUE)
unsupMI <- function(Impute = FALSE, Impute.m = 5, cens.data.lod = NULL,
                    cens.standards = NULL, cens.mice.log = 10,
                    data, log.data = FALSE, algo = "km", k.crit = "ch",
                    comb.cons = FALSE, plot.cons = FALSE, return.detail = FALSE,
                    not.to.use = c("time", "status"),
                    cleanup.partition = TRUE, min.cluster.size = 10,
                    level.order = NULL, Unclassified = "Unclassified"){

  if(Impute == "MImpute"){
    data.imp <-  MImpute(data = data[[1]], mi.m = Impute.m)
    X <- data.imp
  }
  if(Impute == "MImpute_surv"){
    data.imp <-  MImpute_surv(data = data[[1]], mi.m = Impute.m)
    X <- data.imp
  }
  if(Impute == "MImpute_lcens"){
    data.imp <-
      MImpute_lcens(data = data[[1]], mi.m = Impute.m, data.lod = cens.data.lod,
                    standards = cens.standards, mice.log = cens.mice.log)
    X <- data.imp
  }

  if(Impute == FALSE){
    X <- data
  }

  names.use <- setdiff(colnames(data[[1]]), not.to.use)

  Partition.list <- lapply(1:length(X), function(i){
    partition_generation(data = X[[i]][,  names.use],
                        LOG = log.data, clust.algo = algo, k.crit = k.crit)
  })

  if(length(X) == 1){
    my.part <- Partition.list[[1]]
  } else {
    my.part <- MIclust_mpool(
      list.part = Partition.list,
      plot.MIclust = plot.cons,
      comb.cons = ifelse(length(algo) > 1, comb.cons, FALSE))
  }

  if(cleanup.partition){

    my.part <- sapply(my.part,
                             cleanUp_partition,
                             min.cluster.size = min.cluster.size,
                             level.order = level.order,
                             Unclassified = Unclassified)

  }


  if(return.detail){
    if(length(algo) == 1){
      return(list(Consensus = unlist(unname(my.part)),
                  Detail = Partition.list,
                  Imputed.data = X))
    } else{
      return(list(Consensus = my.part,
                  Detail = Partition.list,
                  Imputed.data = X))
    }

  } else {
    if(length(algo) == 1){
      return(unlist(unname(my.part)))
    } else{
      return(my.part)
    }
  }

}
