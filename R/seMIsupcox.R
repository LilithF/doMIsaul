#' Semisupervised learning for a right censored endpoint
#'
#' @description MultiCons consensus based method for MI-Semisupervised
#'   clustering. The final partition is a consensus of the Pareto-optimal
#'   solutions.
#'
#' @param Impute Boolean. Default is FALSE to indicate that the user performed
#'   the imputation and provides the imputed data. If TRUE, the imputation will
#'   be performed within the call using the \code{MImpute_surv()} function. Note
#'   that if Impute is \code{TRUE}, \code{center.init} is also forced to
#'   \code{TRUE} as the center coordinates may depend on the imputation.
#' @param Impute.m Used only if Impute is \code{TRUE}; number of imputations to
#'   perform
#' @param center.init Either a User supplied List of dataframe containing the
#'   cluster centers coordinates (for example as obtained with
#'   \code{initiate_centers()}, Or \code{TRUE} to initiate the centers within
#'   the call of the function (performed with \code{initiate_centers()}). Note
#'   that if \code{TRUE} a random initialization will be performed. For a finer
#'   tuning of the center initialization the user should generate and provide
#'   the list of centers coordinates.
#' @param center.init.N Used only if \code{center.init} is \code{TRUE}. The
#'   number to initialization to produce. Default to 500.
#' @param center.init.Ks Used only if \code{center.init} is \code{TRUE}. Vector
#'   of number of clusters to generate for the initialization. Default to 2 to
#'    7 clusters.
#' @param X Data, in the form of a list of data.frame(s). The list should be one
#'   length 1 if data are complete or if Impute is TRUE, of should be a list of
#'   imputed dataframes if data are incomplete. If columns named "\code{time}"
#'   and "\code{status}" are present they will be discarded for the clustering.
#' @param CVE.fun string indicating how to calculate the cross validation error
#'   : only \code{LP} is available and stands for linear predictor approach
#'   (using the 'ncvreg' package).
#' @param Y Passed to CVE.fun, Outcome data: should be dataframe or matrix with
#'   2 columns: "time" and "status".
#' @param nfolds Number of folds for cross-validation.
#' @param save.path Path indicating where objectives values for each iteration
#'   should be saved. If null the values are not saved.
#' @param Unsup.Sup.relImp List of weights for the unsupervised and supervised
#'   objectives for the Pareto optimal solution. Default is to use only one set
#'   of weights : same weight.
#' @param plot.cons Logical. Should the consensus tree be plotted?
#' @param return.detail logical. Should the detail of imputation specific
#'   partition be returned, in supplement to the final consensus partition?
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
#' @return A vector containing the final cluster IDs. Or if
#'   \code{return.detail == TRUE}, a list containing \code{Consensus}: the final
#'    cluster ID, \code{Detail}: the clusters obtained for each imputed dataset,
#'   \code{Imputed.data} a list containing the imputed datasets.
#' @export
#' @importFrom Gmedian kGmedian
#' @importFrom aricode ARI
#' @importFrom stats model.matrix
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' cancer <- cancer[, -1]
#' ### With imputation included
#' res <- seMIsupcox(X = list(cancer), Y = cancer[, c("time", "status")],
#'                   Impute = TRUE, Impute.m = 3, center.init = TRUE,
#'                   nfolds = 10, center.init.N = 20)
#'
#' ### With imputation and center initialization not included
#' ## 1 imputation
#' cancer.imp <- MImpute_surv(cancer, 3)
#'
#' ## 2 Center initialization
#' # A low N value is used for example purposes. Higher values should be used.
#' N <- 20
#' center.number <- sample(2:6, size = N, replace = TRUE)
#' the.seeds <- runif(N) * 10^9
#' sel.col <- which(!colnames(cancer) %in% c("time", "status"))
#' inits <- sapply(1:length(cancer.imp), function(mi.i) {
#'  initiate_centers(data = cancer.imp[[mi.i]][, sel.col],
#'                   N = N, t = 1, k = center.number,
#'                   seeds.N = the.seeds)},
#'                 USE.NAMES = TRUE, simplify = FALSE)
#'
#' ## 3 learning
#' \donttest{
#' res1 <- seMIsupcox(X = cancer.imp, Y = cancer[, c("time", "status")],
#'                    Impute = FALSE, center.init = inits, nfolds = 10,
#'                    cleanup.partition = FALSE)
#' res2 <- seMIsupcox(X = cancer.imp, Y = cancer[, c("time", "status")],
#'                   center.init = inits, nfolds = 10)
#' }
seMIsupcox <- function(Impute = FALSE, Impute.m = 5,
                       center.init = TRUE, center.init.N = 500,
                       center.init.Ks = 2:7,
                       X, CVE.fun = "LP", Y, nfolds = 10, save.path = NULL,
                       Unsup.Sup.relImp = list("relImp.55" = c(.5, .5)),
                       plot.cons = FALSE,
                       cleanup.partition = TRUE, min.cluster.size = 10,
                       level.order = NULL, Unclassified = "Unclassified",
                       return.detail = FALSE) {

  if(Impute){
    data.imp <-  MImpute_surv(data = X[[1]], mi.m = Impute.m)
    X <- data.imp
    center.init <- TRUE
  }
  mi.m <- length(X)   # number of imputed datasets
  n <- nrow(X[[1]]) # number of samples
  sel.col <- setdiff(colnames(X[[1]]), c("time", "status"))
  p <- length(sel.col)   # number of dimensions

  if(is.logical(center.init)){
    if(!center.init) {
      stop("center.init must be either a list of center positions of TRUE")
    }
    N <- center.init.N
    Ks <- sample(center.init.Ks, size = N, replace = TRUE)
    seeds.t <- runif(N) * 10^9

    Centers <-   sapply(1:mi.m, function(mi.i) {
      initiate_centers(
        data = X[[mi.i]][, sel.col],
        N = N,
        t = 1,
        k = Ks,
        seeds.N = seeds.t
      )
    }, USE.NAMES = TRUE, simplify = FALSE)

  } else {

    N <- length(center.init[[1]])      # number of initial population
    Centers <- center.init

  }

  CVE.fun <- switch(CVE.fun,
                    "LP" = CVE_LP# ,
                    # "VandVH" = CVE_VandVH,
                    # "basic" = CVE_basic
                    )

  # Partition for each set, each imputed dataset
  error.mi <- expand.grid('mi.i' = 1:mi.m,
                          'center' = 1:N)
  error <- expand.grid('center' = 1:N,
                       "Dominated" = NA)
  # For each center initialization
  for (r in 1:N) {
    # calculate objectives
    for(mi.i in 1:mi.m){
      clust.tmp <- kGmedian(X = X[[mi.i]][, sel.col],
                            ncenters = as.matrix(Centers[[mi.i]][[r]]),
                            nstart = 0)
      part.tmp <- data.frame(Cl = factor(clust.tmp$cluster,
                                         levels = 1:nrow(clust.tmp$centers)),
                             stringsAsFactors = TRUE)
      part <- model.matrix(~ 0 + Cl, part.tmp)
  # Calculate Distance for objective clustering & Re-inject correct partition
      a <- Allocation_Distance(X[[mi.i]][, sel.col], clust.tmp$centers)
      a$Allocation <- unname(part)
      assign(paste("MI", mi.i, ".Cl", r, sep = ""), part)
      ligne.i <- which(error.mi$center == r & error.mi$mi.i == mi.i)
      error.mi[ligne.i, 'Cluster'] <- objective_clustering(a)

      CVE <- tryCatch(CVE.fun(list(partition = part.tmp,
                                   data = Y,
                                   nfolds = nfolds)),
                      error = function(e){
                        CVE.fun(list(partition = part.tmp,
                                     data = Y,
                                     nfolds = nfolds))
                      })

      error.mi[ligne.i, 'Regression'] <- CVE$value
      error.mi[ligne.i, 'Error'] <- CVE$error

    }
    # Then combine objective across imputation
    error[error$center == r, c("Cluster", "Regression")] <-
      rowMeans(data.frame(sapply(1:mi.m, function(mi.i){
        as.numeric(error.mi[error.mi$center == r & error.mi$mi.i == mi.i,
                            c("Cluster", "Regression")])
      }, simplify = TRUE, USE.NAMES = TRUE), stringsAsFactors = TRUE))

  }
  # Find pareto non optimal solutions
  mi.final.ojectives.values <- pareto(error)
  if(!is.null(save.path)){
    save(error, error.mi, mi.final.ojectives.values,
         file = paste0(save.path, "__MultiCons_all.Rdata"))
  }
  # Last select final partition set
  optimal.final.obj <-
    mi.final.ojectives.values[mi.final.ojectives.values$Dominated == 0, ]
  # If several optimal, find the ideal
  if(nrow(optimal.final.obj)>1){
    optimal.final.obj.sc <-
      data.frame(scale(optimal.final.obj[, c("Cluster", "Regression")]),
                 stringsAsFactors = TRUE)
    if(length(unique(optimal.final.obj$Cluster)) == 1) {
      optimal.final.obj.sc$Cluster <- 0
    }
    if(length(unique(optimal.final.obj$Regression)) == 1) {
      optimal.final.obj.sc$Regression <- 0
    }
    ideal.optimal <- apply(optimal.final.obj.sc, 2, min)
    optimal.final.objS <- lapply(Unsup.Sup.relImp, function(relImp){
      i.o <- relImp * ideal.optimal
      similarities <- apply(optimal.final.obj.sc, 1, function(x){
        (x %*% i.o) / (t(x) %*% x * t(i.o) %*% i.o)
      })
      optimal.final.obj.relImp <- optimal.final.obj[which.max(similarities), ]
    })
  } else {
    optimal.final.objS <- lapply(Unsup.Sup.relImp, function(x){
      optimal.final.obj
    })
  }
  # And apply consensus to the set to get 1 final partition
  unique.list <- unique(optimal.final.objS)
  cons.partSXX <- sapply(unique.list, function(opt.obj){
    Selected.r <- opt.obj[1, "center"]
    binded.part <- data.frame(do.call(cbind, lapply(1:mi.m, function(mi.i){
      as.character(apply(get(paste("MI", mi.i, ".Cl", Selected.r, sep = "")),
                         1,
                         function(x){which.max(x)}))
    })), stringsAsFactors = TRUE)
    if(mi.m > 1){
      if(all(apply(binded.part[, -1], 2, ARI, c2 = binded.part[, 1]) == 1)){
        # If ARI = 1 between all partitions, no need of consensus function!
        concensus.part <- unname(unlist(binded.part[, 1]))
      } else {
        concensus.part <- MultiCons(DB = binded.part,
                                    Clust_entry = TRUE,
                                    Plot = plot.cons)$Partitions
      }
    } else {
      concensus.part <- unname(unlist(binded.part))
    }

    return(list(list(concensus.part, list(binded.part))))

  }, simplify = TRUE, USE.NAMES = TRUE)
  binded.part <- sapply(cons.partSXX, function(x){x[[2]]},
                        simplify = TRUE, USE.NAMES = TRUE)
  cons.partS <- sapply(cons.partSXX, function(x){x[[1]]},
                       simplify = TRUE, USE.NAMES = TRUE)

  if(length(unique.list) < length(optimal.final.objS)){
    tt <- sapply(unique.list, function(x){x$center},
                 simplify = TRUE, USE.NAMES = TRUE)
    what <- sapply(names(Unsup.Sup.relImp), function(x){
      which(tt == optimal.final.objS[[x]]$center)
    }, simplify = TRUE, USE.NAMES = TRUE)
  } else {
    what <- 1:length(Unsup.Sup.relImp)
    names(what) <- names(Unsup.Sup.relImp)
  }
  ret.part <- lapply(what, function(x){
    factor(cons.partS[, x])
  })

  if(cleanup.partition){

    ret.part <- lapply(ret.part,
                       cleanUp_partition,
                       min.cluster.size = min.cluster.size,
                       level.order = level.order,
                       Unclassified = Unclassified)

  }

  if(return.detail){
    return(list(Consensus = ret.part,
                Detail = binded.part,
                Imputed.data = X))
  } else {
    return(ret.part)
  }
}
