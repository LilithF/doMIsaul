#' Evaluation of a semisupervised obtained partition in comparison to reference
#'   partitions
#'
#' Evaluate number of clusters, ARI, AUC difference, c-index and CPE, with
#'  Supervised, unsupervised and Semisupervised reference  partitions
#'
#' @param partition Vector containing cluster ids of the partition to evaluate.
#' @param ref.unsup Vector: Unsupervised reference partition (i.e. data
#'   structure).
#' @param ref.sup Vector: Supervised reference partition (i.e. using survival
#'   parameters).
#' @param ref.semisup Vector: Semisupervised reference partition (i.e. combining
#'    both).
#' @param data.surv dataframe with variables \code{time} and \code{status}.
#' @param TMIN time point to start analyzing AUC.
#' @param TMAX time point to analyze AUC.
#'
#' @return a list of named performances values
#' @import survival
#' @export
#'
#' @examples
#' library(survival) # survival should be loaded in the environment
#' data(cancer, package = "survival")
#' cancer$status <- cancer$status - 1
#' res <- evaluate_partition_semisup (
#'   partition = factor(rep(c(1,2,3), each = 50)),
#'   ref.unsup = factor(rep(c(1,2,3), times = c(100, 25, 25))),
#'   ref.sup = factor(rep(c(1,2), times = c(50, 100))),
#'   ref.semisup = factor(rep(c(3, 2, 1), times = c(120, 10, 20))),
#'   data.surv = cancer[1:150, c("time", "status")] )
evaluate_partition_semisup <-
  function(partition, ref.unsup, ref.sup, ref.semisup,
           data.surv, TMIN = 2, TMAX = 5){

    requireNamespace("survival", quietly = FALSE)

    data <- data.frame(data.surv,
                       Cluster = factor(partition),
                       Unsup = ref.unsup,
                       Sup = ref.sup,
                       Latent = ref.semisup)
    CC <- complete.cases(data$Cluster)
    data.cc <- data[CC, ]

    # Estimated number of clusters
    Nbclust <- length(levels(data$Cluster[drop = TRUE]))

    # Cluster structure ####
    ARI.Latent <- aricode::ARI(data$Cluster, data$Latent)
    ARI.Unsup <- aricode::ARI(data$Cluster, data$Unsup)

    # Association partition - survival data ####
    if(nlevels(data$Cluster) == 1){data$Cluster <- 1}

    cox.part <-
      survival::coxph(survival::Surv(time, status) ~  Cluster, data.cc)
    cox.latent <-
      survival::coxph(survival::Surv(time, status) ~  Latent, data.cc)
    cox.sup <-
      survival::coxph(survival::Surv(time, status) ~  Sup, data.cc)

    data.cc$LP <-
      suppressWarnings(predict(cox.part, type = "lp"))
    data.cc$LP.latent <-
      suppressWarnings(predict(cox.latent, type = "lp"))
    data.cc$LP.sup <-
      suppressWarnings(predict(cox.sup, type = "lp"))
    timeS <- seq(from = TMIN, to = TMAX, length.out = 100)[-1]

    auc <-
      Extract_AUC(data = data.cc, predictor = "LP", time.vect = timeS)
    auc.latent <-
      Extract_AUC(data = data.cc, predictor = "LP.latent", time.vect = timeS)
    auc.sup <-
      Extract_AUC(data = data.cc, predictor = "LP.sup", time.vect = timeS)

    cindLat.only <-
      Hmisc::rcorrp.cens(data.cc$LP, data.cc$LP.latent,
                         survival::Surv(data.cc$time, data.cc$status),
                         method = 2)
    cindSup.only <-
      Hmisc::rcorrp.cens(data.cc$LP, data.cc$LP.sup,
                         survival::Surv(data.cc$time, data.cc$status),
                         method = 2)

    cpe.latent <- CPE::phcpe(cox.latent)$CPE
    cpe.part <- CPE::phcpe(cox.part)$CPE
    cpe.sup <- CPE::phcpe(cox.sup)$CPE
    # out ####
    ret <- list(Nbclust = Nbclust,
                N.classified = sum(CC),

                DeltaAUCLatent.Tmax = auc$AUC.Tmax - auc.latent$AUC.Tmax,
                DeltaAUCLatent.med = auc$AUC.med - auc.latent$AUC.med,
                DeltaAUCSup.Tmax = auc$AUC.Tmax - auc.sup$AUC.Tmax,
                DeltaAUCSup.med = auc$AUC.med - auc.sup$AUC.med,

                AUCLatent.Tmax = auc.latent$AUC.Tmax,
                AUCLatent.med = auc.latent$AUC.med,
                AUCSup.Tmax = auc.sup$AUC.Tmax,
                AUCSup.med = auc.sup$AUC.med,

                ARI.Latent = ARI.Latent,
                ARI.Unsup = ARI.Unsup,

                cind.Part = cindLat.only["C X1"],
                cind.Lat = cindLat.only["C X2"],
                cind.Sup = cindSup.only["C X2"],
                cindPart.NotLat = cindLat.only["x1 more concordant"],
                cindPart.NotSup = cindSup.only["x1 more concordant"],
                cindLat.NotPart = cindLat.only["x2 more concordant"],
                cindSup.NotPart = cindSup.only["x2 more concordant"],

                cpe.Latent = cpe.latent,
                cpe.Part = cpe.part,
                cpe.Sup = cpe.sup

    )

    return(ret)

  }
