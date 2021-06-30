# Simulation <- function(df.param, where,
#                        md.type = c("MCAR"),
#                        DataSimu = F,
#                        ampute_NA = F,
#                        impute_NA = F,
#                        InitiateCenters = F,
#                        Clustering = F,
#                        UpdatePartitions = F,
#                        PerformancesEvaluation = F,
#                        simu = "new",
#                        ResDoss = "Simulation") {
#   # Performs a simulation using parameters of df.param
#
#   # df.param: dataframe of 1 observation containing the scenario parameters
#   #  as generated in Simulation_parameters.R
#   # where: path indicating parent floder to save results of the simulation
#   # ResDoss: path of grand-parent floder to save results of the simulation
#   # md.type:types of missing data mechanism to evaluate
#   # simu : either "new" or a number for a specific simulation
#   # Other parameters: Plot_res: set to T/F to perform (or not) the step
#
#   # Intit ####
#   if (simu == "new") {
#
# simu.list <- dir(paste(ResDoss, where, sep = "/"),
#                  pattern = "s[[:digit:]]")
#
#     if (length(simu.list) < 1){
#       f <- 1
#
#     } else {
#
#       # Existing simulations: add the first missing
#       num <- as.numeric(gsub("s", "", simu.list))
#       f <- setdiff(1:max(num), num)[1]
#       if (is.na(f)) f <- max(num) + 1
#     }
#
#   } else {
#
#     f <- simu
#
#   }
#
#   dir.create(paste(ResDoss, where, paste("s", f, sep = ""), sep = "/"),
#              showWarnings = F)
#   Dossier <- paste(ResDoss, where, paste("s", f, sep = ""), sep = "/")
#
#
#   # Set parameters####
#   for (l in names(df.param)) {
#     assign(x = l, value = eval(parse(text = df.param[l])))
#   }
#
#   set.seed(10 * f)
#   RNG_save <- runif(9) * 10 ^ 9
#
#   if (!exists("verbose")) verbose <- F
#
#   if (length(grep("Seeds", dir(Dossier))) > 0) {
#
#     load(paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   } else {
#
#     ss <- as.list(RNG_save)
#     names(ss) <- paste(
#       c("DataSimu", "ampute_NA", "impute_NA",
#         "InitiateCenters", "Clustering",
#         "ComputeObjectives", "ParetoOptimality",
#         "Consensus", "PerformancesEvaluation"),
#       c("Start"), sep = ".")
#   }
#
#   # 0a ####
#   if (DataSimu){
#
#     set.seed(ss[["DataSimu.Start"]])
#     if (verbose) cat("0a. Data Generation... \n")
#
    # tmp <- DataSurvGeneration(n = n, means = means, covs = covs,
    #                           k.latent = unlist(k.latent),
    #                           beta = beta_and_lambda[[1]],
    #                           lambda = beta_and_lambda[[2]])
#
#     db <- tmp[["db"]]
#     group <- tmp[["group"]]
#     latent.group <- tmp[["latent.group"]]
#
#     save(db, file = paste(Dossier, "0_SimulatedData.Rdata", sep = "/"))
#     save(group, file = paste(Dossier, "0_data_Groups.Rdata", sep = "/"))
    # save(latent.group, file = paste(Dossier, "0_data_LatentGroups.Rdata",
    #                                 sep = "/"))
#     ss[["DataSimu.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#   # 0b ####
#   if (ampute_NA){
#     set.seed(ss[["ampute_NA.Start"]])
#     if (verbose) cat("0b. Data amputation... \n")
#  if (!exists("db")) load(paste(Dossier, "0_SimulatedData.Rdata", sep = "/"))
#
#     db.NAS <- lapply(md.type, function(md.type.i){
#
#       tt <- MissingDataGeneration(md.type = md.type.i,
#                                   data = db,
#                                   md.prop = md.prop,
#                                   bycases = bycases,
#                                   md.patt = md.patt,
#                                   md.patt.freq = md.patt.freq,
#                                   md.patt.weights = md.patt.weights,
#                                   log = F)
#       colnames(tt) <- colnames(db)
#
#       ss[[paste0("ampute_NA.", toupper(md.type.i), "End")]] <<- .Random.seed
#
#       return(tt)
#
#     })
#
#     names(db.NAS) <- toupper(md.type)
#
#     save(db.NAS, file = paste(Dossier, "0_SimulatedData_NAs.Rdata", sep = "/"))
#     ss[["ampute_NA.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#   # 1 ####
#   if (impute_NA){
#
#     set.seed(ss[["impute_NA.Start"]])
#     if (verbose) cat("1. Data imputation... \n")
#
    # if (!exists("db.NAS")) load(paste(Dossier, "0_SimulatedData_NAs.Rdata",
    #                                   sep = "/"))
#
#     db.imputedS <- sapply(names(db.NAS), function(NA.t){
#
#       if (verbose) cat(paste0("...Missing Mechanism ", NA.t, "...\n"))
#
#       tmp.imputed <- MImpute.surv(data = db.NAS[[NA.t]], mi.m = mi.m)
#
#       who <- paste0("impute_NA.", toupper(NA.t), "End")
#       ss[[who]] <<- .Random.seed
#
#       return(tmp.imputed)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(db.imputedS,
#          file = paste(Dossier, "1_SimulatedData_NA_Imputeds.Rdata", sep = "/"))
#     ss[["impute_NA.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#   # 2 ####
#   if (InitiateCenters){
#
#     set.seed(ss[["InitiateCenters.Start"]])
#     if (verbose) cat("2. Initiate cluster centers... \n")
#     if (!exists("db"))
#       load(paste(Dossier, "0_SimulatedData.Rdata", sep = "/"))
#     if (!exists("db.imputedS"))
#       load(paste(Dossier, "1_SimulatedData_NA_Imputeds.Rdata", sep = "/"))
#
    # n.centers <- sample(as.numeric(names(k.list)), size = N,
    #                     prob = unlist(k.list), replace = T)
#
#     Init.centerS <- sapply(clust.init.t, function(weight.t){
#
      # if (verbose) cat(paste0("...Proportion of random: ", 100 *
      #                           weight.t, "%... \n"))
#       # Generate seeds, and algo for t!= 1
      # seeds.t <- runif(N) * 10 ^ 9
      # Algo.t <- sample(c("km", "hclust.mean", "hclust.med", "kmed"),
      #                  N*(1-weight.t), replace = T)
#
#       if (verbose) cat(paste0("......No Missing data... \n"))
#       who <- paste0("InitiateCenters.t", weight.t, "NoNA", ".Start")
#       ss[[who]] <<- .Random.seed
#
#       ttt <- c(list("NoNA" =
#                       list(Initiate.centers(data = db[, c("X1", "X2", "X3")],
#                                             t = weight.t,
#                                             k = n.centers,
#                                             N = N,
#                                             algorithms = Algo.t,
#                                             seeds.N = seeds.t))),
#                sapply(names(db.imputedS), function(NA.t){
#                  if (verbose) cat(paste0("......Missing Mechanism ", NA.t, "... \n"))
#                  sapply(1:mi.m, function(mi.i){
#
#                    who <- paste0("InitiateCenters.t", weight.t, toupper(NA.t), mi.i, ".Start")
#                    ss[[who]] <<- .Random.seed
#
#                    tt <- Initiate.centers(data = db.imputedS[[NA.t]][[mi.i]][, c("X1", "X2", "X3")],
#                                           t = weight.t,
#                                           k = n.centers,
#                                           N = N,
#                                           algorithms = Algo.t,
#                                           seeds.N = seeds.t)
#                    return(tt)
#                  }, USE.NAMES = T, simplify = F)
#                }, USE.NAMES = T, simplify = F))
#
#       who <- paste0("InitiateCenters.t", weight.t, ".End")
#       ss[[who]] <<- .Random.seed
#       return(ttt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     names(Init.centerS) <- paste("t =", clust.init.t)
#
#     save(Init.centerS,
#          file = paste(Dossier, "2_CenterInitialisations.Rdata", sep = "/"))
#
#     ss[["InitiateCenters.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#   # 3 ####
#   if (Clustering){
#
#     set.seed(ss[["Clustering.Start"]])
#     if (verbose) cat("3. Cluster allocation... \n")
#     if (!exists("db"))
#       load(paste(Dossier, "0_SimulatedData.Rdata", sep = "/"))
#     if (!exists("db.imputedS"))
#       load(paste(Dossier, "1_SimulatedData_NA_Imputeds.Rdata", sep = "/"))
#     if (!exists("Init.centerS"))
#       load(paste(Dossier, "2_CenterInitialisations.Rdata", sep = "/"))
#
#     Obj.path <- paste(Dossier, "ObjectivesValues", sep = "/")
#     dir.create(Obj.path, showWarnings = F)
#
#     tmp <- sapply(names(Init.centerS), function(weight.t){
#
#       if (verbose) cat(paste0("...Proportion of random: ", weight.t, "... \n"))
#       t.name <- gsub(" = ", "", weight.t, fixed = T)
#
#       aa <- sapply(names(Init.centerS[[weight.t]]), function(NA.t){
#
#         if (verbose) cat(paste0("......Missing Mechanism ", NA.t, "... \n"))
#         if (NA.t == "NoNA"){data.temp <- list(db)} else {data.temp <- db.imputedS[[NA.t]]}
#
#         a <- sapply(CVE, function(CVE.t){
#           CVE.temp <- switch(CVE.t,
#                              "CVE.1" = CVE1_basic,
#                              "CVE.2" = CVE2_VandVH,
#                              "CVE.3" = CVE3_LinearPred)
#           CVE.Warnings <- function(x){
#             r <-
#               withCallingHandlers({
#                 error_text <- NA
#                 list(value = CVE.temp(partition = x$partition,
#                                       data = x$data,
#                                       nfolds = x$nfolds),
#                      error = error_text)
#               },
#               warning = function(e) {
#                 error_text <<- 1
#                 invokeRestart("muffleWarning")
#               }
#               )
#
#             return(r)
#           }
#
#           if(Khorshidi.MI || NA.t == "NoNA"){
#             methodo.type.tmp <- methodo.type
#           } else {
#             methodo.type.tmp <- methodo.type[grep("MultiCons", methodo.type)]
#           }
#           ret0 <- sapply(methodo.type.tmp, function(method.t){
#
#             if (verbose) cat(".........", CVE.t, method.t, "...\n")
#             tt <- switch(
#               method.t,
#               "MultiCons" = Consensus.SemiSup_clustring(
#                 center.init = Init.centerS[[weight.t]][[NA.t]],
#                 X = data.temp,
#                 CVE.fun = CVE.Warnings,
#                 Y = db[, c("time", "status")],
#                 nfolds = cv.nfolds,
#                 save.path = paste0(Obj.path,"/",t.name,"_", NA.t, "_", CVE.t),
#                 Unsup.Sup.relImp = Unsup.Sup.relImp),
#               "Khorshidi.1" = Khorshidi_clustring(
#                 update.all.IMversion = T,
#                 center.init = Init.centerS[[weight.t]][[NA.t]],
#                 termination.criteria = taumax,
#                 ETA = ETA,
#                 X = data.temp,
#                 CVE.fun = CVE.Warnings,
#                 Y = db[, c("time", "status")],
#                 nfolds = cv.nfolds,
#                 save.path = paste0(Obj.path,"/",t.name,"_", NA.t, "_", CVE.t)),
#               "Khorshidi.2" = Khorshidi_clustring(
#                 update.all.IMversion = F,
#                 center.init = Init.centerS[[weight.t]][[NA.t]],
#                 termination.criteria = taumax,
#                 ETA = ETA,
#                 X = data.temp,
#                 CVE.fun = CVE.Warnings,
#                 Y = db[, c("time", "status")],
#                 nfolds = cv.nfolds,
#                 save.path = paste0(Obj.path,"/",t.name,"_", NA.t, "_", CVE.t)),
#               "MultiCons.unsup" = Consensus.UnSup_clustring(data = data.temp)
#             )
#
#             who <- paste0("Clustering.", weight.t, toupper(NA.t), CVE.t, method.t, ".End")
#             ss[[who]] <<- .Random.seed
#             gc()
#
#             return(tt)
#           }, USE.NAMES = T, simplify = T)
#           if("MultiCons" %in% methodo.type.tmp){
#             which.multi <- which(methodo.type.tmp %in% "MultiCons")
#             ll <- length(ret0[[which.multi]])
#             ret0 <- c(c(ret0[[which.multi]], ret0[-which.multi]))
#             names(ret0)[1:ll] <- paste0("MultiCons", names(ret0)[1:ll])
#             ret0 <- do.call(cbind, ret0)
#           }
#
#           ret0
#
#         }, USE.NAMES = T, simplify = F)
#         ret <- do.call(cbind, a)
#         colnames(ret) <- paste(rep(CVE, each = ncol(a[[1]])), colnames(ret), sep = "_")
#         return(ret)
#       }, USE.NAMES = T, simplify = F)
#       ret2 <- do.call(cbind, aa)
#       colnames(ret2) <- paste(
#         unlist(sapply(1:length(Init.centerS[[weight.t]]), function(x){
#           rep(names(Init.centerS[[weight.t]])[x], ncol(aa[[x]]))})),
#         colnames(ret2), sep = "_")
#       return(ret2)
#     }, USE.NAMES = T, simplify = F)
#     PartitionS <- do.call(cbind, tmp)
#     colnames(PartitionS) <- paste(rep(gsub(" = ", "", names(Init.centerS)), each = ncol(tmp[[1]])),
#                                   colnames(PartitionS), sep = "_")
#     save(PartitionS, file = paste(Dossier, "3_PartitionS.Rdata", sep = "/"))
#
#     ss[["Clustering.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#   # 3bis ####
#   if (UpdatePartitions){
#
#     if (verbose) cat("3bis. Update Partitons... \n")
#     if (!exists("PartitionS"))
#       load(paste(Dossier, "3_PartitionS.Rdata", sep = "/"))
#
#     PartitionS.updated <- data.frame(sapply(data.frame(PartitionS), function(part.t){
#       smr <- summary(factor(part.t), maxsum = 600)
#       to.del <- names(smr)[smr < Upd.part]
#       if(length(to.del)>0) {
#         part.t[part.t %in% to.del] <- NA
#       }
#       part.t
#     }, USE.NAMES = T))
#
#     save(PartitionS.updated, file = paste(Dossier, "3bis_PartitionSUpdated.Rdata", sep = "/"))
#
#   }
#   # 4 ####
#   if (PerformancesEvaluation){
#
#     set.seed(ss[["PerformancesEvaluation.Start"]])
#     if (verbose) cat("4. Performances Evaluation... \n")
#     if (!exists("PartitionS.updated"))
#       load(paste(Dossier, "3bis_PartitionSUpdated.Rdata", sep = "/"))
#     if (!exists("PartitionS"))
#       load(paste(Dossier, "3_PartitionS.Rdata", sep = "/"))
#     if (!exists("db"))
#       load(paste(Dossier, "0_SimulatedData.Rdata", sep = "/"))
#     if (!exists("group"))
#       load(paste(Dossier, "0_data_Groups.Rdata", sep = "/"))
#     if (!exists("latent.group"))
#       load(paste(Dossier, "0_data_LatentGroups.Rdata", sep = "/"))
#
#     beta.tmp <- rank(c(0, beta_and_lambda[[1]]), ties.method = c("average"))
#     sup.group <- plyr::mapvalues(latent.group, levels(latent.group), beta.tmp)
#
#     part.qualS <- sapply(colnames(PartitionS), function(part.t){
#       suffix <- strsplit(part.t, "_")[[1]][4]
#       tt <- PerformancesEvaluation(partition = PartitionS.updated[, part.t],
#                                    true.unsup = group,
#                                    true.sup = sup.group,
#                                    true.latent = latent.group,
#                                    data.surv = db[, c("time", "status")],
#                                    TMIN = TMIN,
#                                    TMAX = TMAX)
#
#       who <- paste0("Eval_partitions.", part.t, ".End")
#       ss[[who]] <<- .Random.seed
#
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(part.qualS, file = paste(Dossier, "4_PerformancesEvaluation.Rdata", sep = "/"))
#
#     ss[["PerformancesEvaluation.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#   }
#
#   return(NULL)
#
# }
#
# DataSurvGeneration <- function(n, means = list(rep(0, 2)), covs = list(diag(rep(1, 2))),
#                                k.latent, beta, lambda){
#
#   # Generates a dataset with latent clusters according to multivariate
#   #  Gaussian distributions and time to event data
#   # (exponential distribution distibution for both latent and censor time)
#
#   # n: number of observations
#   # means: list of means parameter. each element coresspond to a group
#   # covs: list of cov parameter. each element coresspond to a group
#   #  for means and covs, list must be of size of k.latent
#   #  if list of size 1 submitted the value will be repeated for each group
#   # k.latent: named vector of proportion of observations in each latent cluster
#   #           the names are structured as follow : "A.I" where A is the index of
#   #           the observerd cluster label (numeric) and I is the index of the latent cluster.
#   # beta: for each Latent cluster except the first hazard ratio (with Group Latent 1 = ref)
#   # lambda: parameter for teh exponential distribution for censor time
#
#   id.group <- sapply(names(k.latent), function(x){
#     as.numeric(strsplit(x, ".", fixed = T)[[1]][1])
#   }, USE.NAMES = F)
#
#   k <- length(unique(id.group))
#
#   if (!is.list(means) | !is.element(length(means), c(1, k))){
#     stop("means must be a list of length 1 or k")
#   }
#
#   if (!is.list(covs) | !is.element(length(covs), c(1, k))){
#     stop("corr must be a list of length 1 or k")
#   }
#
#   if (length(means) == 1){
#     means <- rep(means, k)
#   }
#
#   if (length(covs) == 1){
#     covs <- rep(covs, k)
#   }
#
#
#   # Covariates
#   db <- do.call(
#     rbind,
#     lapply(1:k, function(i){
#       ni <- round(n * sum(k.latent[id.group == i]), 0)
#       tmp <- data.frame(MASS::mvrnorm(ni,
#                                       mu = means[[i]],
#                                       Sigma = covs[[i]]),
#                         group = i,
#                         g.latent = i)
#       if(sum(id.group == i)>1){
#         tmp$g.latent[tmp$X1 > median(tmp$X1)] <- names(k.latent[id.group == i])[2]
#       }
#       tmp
#     })
#   )
#
#   db$group <- factor(db$group)
#   db$g.latent <- factor(db$g.latent)
#
#   # Survival data
#   X <- model.matrix(~ 0 + ., data = data.frame(part = db$g.latent))[, -1]
#   X.beta <- t(apply(X, 1, function(x){x*beta}))
#   # Latent time
#   myrates <- .2* exp(rowSums(X.beta + 1/10)) 		# risque
#   y <- rexp(n, rate = myrates)
#   # Censor times
#   cen <- 1.8 * rexp(n, rate = lambda)
#   # follow-up times and event indicators
#   time <- pmin(y, cen)
#   status <- as.numeric(y <= cen)
#
#   db.fin <- data.frame(time = time, status = status, db)
#
#   return(list(db = db.fin[, setdiff(colnames(db.fin), c("group", "g.latent"))],
#               group = db.fin$group,
#               latent.group = db.fin$g.latent))
# }
#
# MissingDataGeneration <- function(data, md.type, md.prop = 0.2, bycases = T,
#                                   dep = NULL, md.patt = NULL,
#                                   md.patt.freq = NULL,
#                                   md.patt.weights = NULL, log = T){
#   # Generates missing data according the desired mechanism
#   #  Using a modifed version of the mice::amupte function
#
#   # data: data.frame
#   # md.type: mechanism (mcar, mar or mnar)
#   # md.prop: proportion of missing data
#   # bycases: is the propotion defined by cases (T) or cells (F)
#   # dep: value of dependence parameter (not required for MCAR)
#   # md.patt: pattern of missing data (as in mice::ampute, pattern)
#   # md.patt.freq: frequence of eah pattern
#   # md.patt.weights = named list of pattern weight (as in mice::ampute, weights)
#   #  with one element named by md.type value
#   # log: hould the data be logged before amputing teh data (T is recommended
#   #   in case of log-noraml distribution)
#
#   if(log){data.tmp <- unname(log(data))} else {data.tmp <- unname(data)}
#
#   db <- ampute.modified(data = data.tmp, prop = md.prop, cont = T,
#                         std = T, odds = NULL, patterns = md.patt,
#                         freq = md.patt.freq, weights = md.patt.weights[[md.type]],
#                         mech = gsub("WK", "", md.type), bycases = bycases, dep = dep)$amp
#   if(log){db <- exp(db)}
#
#   return(data.frame(db))
#
# }
#
# ampute.modified <- function (data, prop = 0.5, patterns = NULL,
#                              freq = NULL, mech = "MAR",
#                              weights = NULL, std = TRUE,
#                              cont = TRUE, type = NULL, odds = NULL,
#                              bycases = TRUE, run = TRUE, dep = NULL){
#   # Modified version of mice::amupte where ampute.continuous function call
#   #  has been replaced by ampute.continuous.modified function call and
#   #  the scores calcultaed by sum.scores are multiplied by the dep
#   #  parameter
#
#   sum.scores <- function(P, data, std, weights, dep) {
#     weights <- as.matrix(weights)
#     f <- function(i) {
#       if (length(P[P == (i + 1)]) == 0) {
#         return(0)
#       } else {
#         candidates <- as.matrix(data[P == (i + 1), ])
#         if (std) {
#           length_unique <- function(x) {
#             return(length(unique(x)) == 1)
#           }
#           if (!(nrow(candidates) > 1 && any(apply(candidates,
#                                                   2, length_unique)))) {
#             candidates <- scale(candidates)
#           }
#         }
#         scores <- apply(candidates, 1, function(x){
#           weights[i, ] %*% x
#         })
#         scores <- dep[i] * scores
#         return(scores)
#       }
#     }
#     scores <- lapply(seq_len(nrow(patterns)), f)
#     return(scores)
#   }
#   recalculate.prop <- function(prop, n, patterns, freq) {
#     miss <- prop * n ^ 2
#     cases <- vapply(seq_len(nrow(patterns)),
#                     function(i) (miss * freq[i]) /
#                       length(patterns[i, ][patterns[i, ] == 0]),
#                     numeric(1))
#     if (sum(cases) > n) {
#       stop("Proportion of missing cells is too large in combination
#            with the desired number of missing variables",
#            call. = FALSE)
#     }
#     else {
#       prop <- sum(cases) / n
#     }
#     return(prop)
#   }
#   recalculate.freq <- function(freq) {
#     return(freq / sum(freq))
#   }
#   check.patterns <- function(patterns, freq, prop) {
#     prop.one <- 0
#     row.one <- c()
#     for (h in seq_len(nrow(patterns))) {
#       if (any(!patterns[h, ] %in% c(0, 1))) {
#         stop(paste("Argument patterns can only contain 0 and 1, pattern",
#                    h, "contains another element"), call. = FALSE)
#       }
#       if (all(patterns[h, ] %in% 1)) {
#         prop.one <- prop.one + freq[h]
#         row.one <- c(row.one, h)
#       }
#     }
#     if (prop.one != 0) {
#       warning(paste("Proportion of missingness has changed from",
#                     prop, "to", prop.one,
#                     "because of pattern(s) with merely ones"),
#               call. = FALSE)
#       prop <- prop.one
#       freq <- freq[-row.one]
#       freq <- recalculate.freq(freq)
#       patterns <- patterns[-row.one, ]
#       warning("Frequency vector and patterns matrix have
#               changed because of pattern(s) with merely ones",
#               call. = FALSE)
#     }
#     prop.zero <- 0
#     row.zero <- c()
#     for (h in seq_len(nrow(patterns))) {
#       if (all(patterns[h, ] %in% 0)) {
#         prop.zero <- prop.zero + freq[h]
#         row.zero <- c(row.zero, h)
#       }
#     }
#     objects = list(patterns = patterns, prop = prop, freq = freq,
#                    row.zero = row.zero)
#     return(objects)
#   }
#   if (is.null(data)) {
#     stop("Argument data is missing, with no default", call. = FALSE)
#   }
#   data <-  mice:::check.dataform(data)
#   if (anyNA(data)) {
#     stop("Data cannot contain NAs", call. = FALSE)
#   }
#   if (ncol(data) < 2) {
#     stop("Data should contain at least two columns", call. = FALSE)
#   }
#   data <- data.frame(data)
#   if (any(vapply(data, Negate(is.numeric), logical(1))) &&
#       mech != "MCAR") {
#     data <- as.data.frame(sapply(data, as.numeric))
#     warning("Data is made numeric because the calculation of weights requires numeric data",
#             call. = FALSE)
#   }
#   if (prop < 0 || prop > 100) {
#     stop("Proportion of missingness should be a value between 0 and 1 (for a proportion) or between 1 and 100 (for a percentage)",
#          call. = FALSE)
#
#   } else if (prop > 1) {
#     prop <- prop / 100
#   }
#   if (is.null(patterns)) {
#     patterns <- ampute.default.patterns(n = ncol(data))
#   }   else if (is.vector(patterns) && (length(patterns) / ncol(data)) %% 1 ==
#                0) {
#     patterns <- matrix(patterns, length(patterns) / ncol(data),
#                        byrow = TRUE)
#     if (nrow(patterns) == 1 && all(patterns[1, ] %in% 1)) {
#       stop("One pattern with merely ones results to no amputation at all, the procedure is therefore stopped",
#            call. = FALSE)
#     }
#   }   else if (is.vector(patterns)) {
#     stop("Length of pattern vector does not match #variables",
#          call. = FALSE)
#   }
#   patterns <- data.frame(patterns)
#   if (is.null(freq)) {
#     freq <- ampute.default.freq(patterns = patterns)
#   }
#   if (!is.vector(freq)) {
#     freq <- as.vector(freq)
#     warning("Frequency should be a vector", call. = FALSE)
#   }
#   if (length(freq) != nrow(patterns)) {
#     if (length(freq) > nrow(patterns)) {
#       freq <- freq[seq_along(nrow(patterns))]
#     }
#     else {
#       freq <- c(freq, rep.int(0.2, nrow(patterns) - length(freq)))
#     }
#     warning(paste("Length of vector with relative frequencies does not match #patterns and is therefore changed to",
#                   freq), call. = FALSE)
#   }
#   if (sum(freq) != 1) {
#     freq <- recalculate.freq(freq = freq)
#   }
#   if (!bycases) {
#     prop <- recalculate.prop(prop = prop, freq = freq, patterns = patterns,
#                              n = ncol(data))
#   }
#   check.pat <- check.patterns(patterns = patterns, freq = freq,
#                               prop = prop)
#   patterns.new <- check.pat[["patterns"]]
#   freq <- check.pat[["freq"]]
#   prop <- check.pat[["prop"]]
#   if (any(!mech %in% c("MCAR", "MAR", "MNAR"))) {
#     stop("Mechanism should be either MCAR, MAR or MNAR",
#          call. = FALSE)
#   }
#   if (!is.vector(mech)) {
#     mech <- as.vector(mech)
#     warning("Mechanism should contain merely MCAR, MAR or MNAR",
#             call. = FALSE)
#   }   else if (length(mech) > 1) {
#     mech <- mech[1]
#     warning("Mechanism should contain merely MCAR, MAR or MNAR. First element is used",
#             call. = FALSE)
#   }
#   if (!is.null(check.pat[["row.zero"]]) && mech == "MAR") {
#     stop(paste("Patterns object contains merely zeros and this kind of pattern is not possible when mechanism is MAR"),
#          call. = FALSE)
#   }
#   if (mech == "MCAR" && !is.null(weights)) {
#     weights = NULL
#     warning("Weights matrix is not used when mechanism is MCAR",
#             call. = FALSE)
#   }
#   if (mech == "MCAR" && !is.null(odds)) {
#     odds = NULL
#     warning("Odds matrix is not used when mechanism is MCAR",
#             call. = FALSE)
#   }
#   if (mech != "MCAR" && !is.null(weights)) {
#     if (is.vector(weights) && (length(weights) / ncol(data)) %% 1 ==
#         0) {
#       weights <- matrix(weights, length(weights) / ncol(data),
#                         byrow = TRUE)
#     }
#     else if (is.vector(weights)) {
#       stop("Length of weight vector does not match #variables",
#            call. = FALSE)
#     }
#     else if (!is.matrix(weights) && !is.data.frame(weights)) {
#       stop("Weights matrix should be a matrix", call. = FALSE)
#     }
#   }
#   if (is.null(weights)) {
#     weights <- ampute.default.weights(patterns = patterns.new,
#                                       mech = mech)
#   }
#   weights <- as.data.frame(weights)
#   if (!nrow(weights) %in% c(nrow(patterns), nrow(patterns.new))) {
#     stop("The objects patterns and weights are not matching",
#          call. = FALSE)
#   }
#   if (!is.vector(cont)) {
#     cont <- as.vector(cont)
#     warning("Continuous should contain merely TRUE or FALSE",
#             call. = FALSE)
#   }  else if (length(cont) > 1) {
#     cont <- cont[1]
#     warning("Continuous should contain merely TRUE or FALSE. First element is used",
#             call. = FALSE)
#   }
#   if (!is.logical(cont)) {
#     stop("Continuous should contain TRUE or FALSE", call. = FALSE)
#   }
#   if (cont && !is.null(odds)) {
#     odds = NULL
#     warning("Odds matrix is not used when continuous probabilities (cont == TRUE) are specified",
#             call. = FALSE)
#   }
#   if (!cont && !is.null(type)) {
#     type = NULL
#     warning("Type is not used when discrete probabilities (cont == FALSE) are specified",
#             call. = FALSE)
#   }
#   if (is.null(type)) {
#     type <- ampute.default.type(patterns = patterns.new)
#   }
#   if (any(!type %in% c("LEFT", "MID", "TAIL", "RIGHT"))) {
#     stop("Type should contain LEFT, MID, TAIL or RIGHT",
#          call. = FALSE)
#   }
#   if (!is.vector(type)) {
#     type <- as.vector(type)
#     warning("Type should be a vector of strings", call. = FALSE)
#   }  else if (!length(type) %in% c(1, nrow(patterns), nrow(patterns.new))) {
#     type <- type[1]
#     warning("Type should either have length 1 or length equal to #patterns, first element is used for all patterns",
#             call. = FALSE)
#   }
#   if (mech != "MCAR" && !is.null(odds) && !is.matrix(odds)) {
#     if (nrow(patterns.new) == 1 && is.vector(odds)) {
#       odds <- matrix(odds, nrow = 1)
#     }
#     else {
#       stop("Odds matrix should be a matrix", call. = FALSE)
#     }
#   }
#   if (is.null(odds)) {
#     odds <- ampute.default.odds(patterns = patterns.new)
#   }
#   if (!cont) {
#     for (h in seq_len(nrow(odds))) {
#       if (any(!is.na(odds[h, ]) & odds[h, ] < 0)) {
#         stop("Odds matrix can only have positive values",
#              call. = FALSE)
#       }
#     }
#   }
#   if (!nrow(odds) %in% c(nrow(patterns), nrow(patterns.new))) {
#     stop("The objects patterns and odds are not matching",
#          call. = FALSE)
#   }
#
#   if (is.null(dep)) {
#     dep <- rep(1, nrow(patterns))
#   }
#   if (length(dep) != nrow(patterns)) {
#     if (length(dep) > nrow(patterns)) {
#       dep <- dep[seq_along(nrow(patterns))]
#     }
#     else {
#       dep <- rep(dep[1], nrow(patterns))
#     }
#     warning(paste("Length of vector with relative frequencies does not match #patterns and is therefore changed to",
#                   freq), call. = FALSE)
#   }
#
#   P <- NULL
#   scores <- NULL
#   missing.data <- NULL
#
#
#
#   if (run) {
#     P <- sample.int(n = nrow(patterns.new), size = nrow(data),
#                     replace = TRUE, prob = freq) + 1
#     if (mech == "MCAR") {
#       R <- ampute.mcar(P = P, patterns = patterns.new,
#                        prop = prop)
#     } else {
#       scores <- sum.scores(P = P, data = data, std = std,
#                            weights = weights, dep = dep)
#       if (!cont) {
#         R <- ampute.discrete(P = P, scores = scores,
#                              odds = odds, prop = prop)
#       } else if (cont) {
#         R <- ampute.continuous.modified(P = P, scores = scores,
#                                         prop = round(prop, 3), type = type)
#       }
#     }
#     missing.data <- data
#     for (i in seq_len(nrow(patterns.new))) {
#       if (any(P == (i + 1))) {
#         missing.data[R[[i]] == 0, patterns.new[i, ] ==
#                        0] <- NA
#       }
#     }
#   }
#   names(patterns.new) <- names(data)
#   names(weights) <- names(data)
#   call <- match.call()
#   missing.data <- data.frame(missing.data)
#   result <- list(call = call, prop = prop, patterns = patterns.new,
#                  freq = freq, mech = mech, weights = weights, cont = cont,
#                  std = std, type = type, odds = odds, amp = missing.data,
#                  cand = P - 1, scores = scores, data = as.data.frame(data))
#   oldClass(result) <- "mads"
#   return(result)
# }
#
# ampute.continuous.modified <- function (P, scores, prop, type) {
#   # Modifed version of mice::ampute.continuous to evaluate the shift
#   #  without scale in order to control the dependence (parameter dep
#   #  in ampute.modified)
#
#   bin.search <- function(fun, range = c(-8, 8), ..., target = 0,
#                          lower = ceiling(min(range)),
#                          upper = floor(max(range)),
#                          maxiter = 100, showiter = FALSE) {
#     lo <- lower
#     hi <- upper
#     counter <- 0
#     val.lo <- round(fun(lo, ...), 3)
#     val.hi <- round(fun(hi, ...), 3)
#     sign <- if (val.lo > val.hi)
#       -1
#     else 1
#     if (target * sign < val.lo * sign)
#       outside.range <- TRUE
#     else if (target * sign > val.hi * sign)
#       outside.range <- TRUE
#     else outside.range <- FALSE
#     while (counter < maxiter && !outside.range) {
#       counter <- counter + 1
#       if (hi - lo <= (1/(10^3)) || lo < lower || hi >
#           upper)
#         break
#       center <- round((hi - lo)/2 + lo, 3)
#       val <- round(fun(center, ...), 3)
#       if (showiter) {
#         cat("--------------\n")
#         cat("Iteration #", counter, "\n")
#         cat("lo=", lo, "\n")
#         cat("hi=", hi, "\n")
#         cat("center=", center, "\n")
#         cat("fun(lo)=", val.lo, "\n")
#         cat("fun(hi)=", val.hi, "\n")
#         cat("fun(center)=", val, "\n")
#       }
#       if (val == target) {
#         val.lo <- val.hi <- val
#         lo <- hi <- center
#         break
#       }
#       else if (sign * val < sign * target) {
#         lo <- center
#         val.lo <- val
#       }
#       else {
#         hi <- center
#         val.hi <- val
#       }
#       if (showiter) {
#         cat("new lo=", lo, "\n")
#         cat("new hi=", hi, "\n")
#         cat("--------------\n")
#       }
#     }
#     retval <- list(call = match.call(), numiter = counter)
#     if (outside.range) {
#       if (target * sign < val.lo * sign) {
#         warning("The desired proportion of ", target,
#                 " is too small; ", val.lo, " is used instead.")
#         retval$flag = "Lower Boundary"
#         retval$where = lo
#         retval$value = val.lo
#       }
#       else {
#         warning("The desired proportion of ", target,
#                 " is too large; ", val.hi, " is used instead.")
#         retval$flag = "Upper Boundary"
#         retval$where = hi
#         retval$value = val.hi
#       }
#     }
#     else if (counter >= maxiter) {
#       retval$flag = "Maximum number of iterations reached"
#       retval$where = (lo + hi)/2
#       retval$value = (val.lo + val.hi)/2
#     }
#     else if (val.lo == target) {
#       retval$flag = "Found"
#       retval$where = lo
#       retval$value = val.lo
#     }
#     else if (val.hi == target) {
#       retval$flag = "Found"
#       retval$where = hi
#       retval$value = val.hi
#     }
#     else {
#       retval$flag = "Between Elements"
#       retval$where = (lo + hi)/2
#       retval$value = (val.lo + val.hi)/2
#     }
#     return(retval)
#   }
#   logit <- function(x) exp(x)/(1 + exp(x))
#   R <- vector(mode = "list", length = length(scores))
#   if (length(type) == 1) {
#     type <- rep.int(type, length(scores))
#   }
#   for (i in seq_along(scores)) {
#     ######
#     testset <- (sample(scores[[i]], size = 10000, replace = T))
#     formula <- switch(type[i],
#                       LEFT = function(x, b) logit(mean(x) - x + b),
#                       MID = function(x, b) logit(-abs(x - mean(x)) + 0.75 + b),
#                       TAIL = function(x, b) logit(abs(x - mean(x)) - 0.75 + b),
#                       function(x, b) logit(-mean(x) + x + b))
#     shift <- bin.search(fun = function(shift) sum(formula(x = testset,
#                                                           b = shift))/length(testset),
#                         target = prop,
#                         range = c(-8  - mean(scores[[i]]), 8 + mean(scores[[i]])))$where
#
#     if (length(shift) > 1) {
#       shift <- shift[1]
#     }
#
#     scores.temp <- scores[[i]]
#     if (length(scores.temp) == 1 && scores.temp == 0) {
#       R[[i]] <- 0
#     } else {
#       if (length(scores.temp) == 1) {
#         probs <- prop
#       } else if (length(unique(scores.temp)) == 1) {
#         probs <- prop
#       }
#       else {
#         probs <- formula(x = scores.temp, b = shift)
#       }
#       R.temp <- 1 - rbinom(n = length(scores.temp), size = 1,
#                            prob = probs)
#       R[[i]] <- replace(P, P == (i + 1), R.temp)
#       R[[i]] <- replace(R[[i]], P != (i + 1), 1)
#     }
#   }
#   return(R)
# }
#
# # **
# Khorshidi_clustring <- function(update.all.IMversion, center.init,termination.criteria = 1000,
#                                 ETA, X, CVE.fun, Y, nfolds, save.path = NULL){
#   # Wrapper function for Khorshidi's method adapted to MI and Survival frameworks
#   #     Adaptation from the code of Khorshidi et al. 2019
#   # Returns a vector of final cluster ID
#
#   # update.all.IMversion: (for application of the procedure in MI framewrk)  T to
#   #    update combine objectives across imputation and make update decision for all
#   #    m version of the partition at once ("V.Khorshidi.1"), or F to make update
#   #    for each of the m version of the partition independenlty ("V.Khorshidi.2")
#   # center.init: list of dataframe containing the cluster centers coordinates
#   # termination.criteria : number of iterations
#   # ETA: learning rate
#   # X: data (columns names "time" and "status" will be discared for the clustering)
#   # CVE.fun : function to calculate CVE (survival based objective) arguments must be
#   #      data, partition, nfolds
#   # Y: passed to CVE.fun Outcome (survival) should be dataframe or matrix with 2
#   #      columns: "time" and "status"
#   # nfolds: argument passed to CVE.fun
#   # save.path: path indicating where objectives values for each iteration should be save.
#   #      If null the values are not saved.
#
#   mi.m <- length(X)   # number of imputed datasets
#   n <- nrow(X[[1]]) # number of samples
#   sel.col <- setdiff(colnames(X[[1]]), c("time", "status"))
#   p <- length(sel.col)   # number of dimensions
#   N <- length(center.init[[1]])      # number of initial population
#   Centers <- center.init
#   eta <- ETA
#   if (update.all.IMversion) {
#     # V.Khorshidi.1: decision for all imputed dataset
#     error.mi <- expand.grid('mi.i' = 1:mi.m,
#                             'center' = 1:N,
#                             'tau' = 1:termination.criteria)
#     error <- expand.grid('center' = 1:N,
#                          'tau' = 1:termination.criteria,
#                          "Dominated" = NA)
#     tau <- 1 # iteration counter
#     terminate <- FALSE
#     while (!terminate) {
#       # check termination criteria:
#       terminate <- tau >= termination.criteria
#       if (tau == 1) {
#         set.1 <- 1:N
#       } else {
#         set.1 <- error[error$tau == tau - 1 & error$Dominated %in% c(1), "center"]
#       }
#       for (r in set.1) {
#         # calculate objectives
#         for(mi.i in 1:mi.m){
#
#           a <- Allocation_Distance(X[[mi.i]][, sel.col], Centers[[mi.i]][[r]])
#           Cl <- as.data.frame(a$Allocation)
#           colnames(Cl) <- paste("Cl", 1:ncol(Cl), sep = "")
#           assign(paste("MI", mi.i, ".Cl", r, sep = ""), Cl)
#           ligne.i <- which(error.mi$tau == tau & error.mi$center == r & error.mi$mi.i == mi.i)
#           error.mi[ligne.i, 'Cluster'] <- objective_clustering(a)
#           CVE <- tryCatch(CVE.fun(list(partition = data.frame(part = apply(Cl, 1, which.max)),
#                                        data = Y,
#                                        nfolds = nfolds)),
#                           error = function(e){
#                             CVE.fun(list(partition = data.frame(part = apply(Cl, 1, which.max)),
#                                          data = Y,
#                                          nfolds = nfolds))
#                           })
#           error.mi[ligne.i, 'Regression'] <- CVE$value
#           error.mi[ligne.i, 'Error'] <- CVE$error
#         }
#
#         # Then combine objective across imputation
#         error[error$tau == tau & error$center == r, c("Cluster", "Regression")] <-
#           rowMeans(data.frame(sapply(1:mi.m, function(mi.i){
#             as.numeric(error.mi[error.mi$tau == tau & error.mi$center == r & error.mi$mi.i == mi.i,
#                                 c("Cluster", "Regression")])
#           })))
#       }
#       # Find pareto non optimal solutions
#       error[error$tau == tau, ] <- pareto(error[error$tau == tau, ])
#       # Update centers which need update and repeat obj value for non updates
#       for (r in 1:N) {
#         if (error[error$tau == tau & error$center == r, "Dominated"] > 0) {
#           # for each imputed version
#           for(mi.i in 1:mi.m){
#             Z <- X[[mi.i]][, sel.col]
#             Xc <- data.frame(matrix(0, nrow(Centers[[mi.i]][[r]]), p))
#             # for each center:
#             for (j in 1:nrow(Xc)) {
#               # update the coefficient:
#               Zq <- Z[get(paste("MI", mi.i, ".Cl", r, sep = ""))[, j] == 1, ][
#                 sample(nrow(Z[get(paste("MI", mi.i, ".Cl", r, sep = ""))[, j] == 1, ]), 1), ]
#               if (colSums(get(paste("MI", mi.i, ".Cl", r, sep = "")))[j] > 0) {
#                 Xc[j, ] <- Centers[[mi.i]][[r]][j,] +
#                   eta / ((1 + colSums(get(paste("MI", mi.i, ".Cl", r, sep = "")))[j]) ^ 0.75) *
#                   (Zq - Centers[[mi.i]][[r]][j,]) / sum(abs(Zq - Centers[[mi.i]][[r]][j,]))
#               } else {
#                 Xc[j,] <- Centers[[mi.i]][[r]][j,]
#               }
#               if (any(is.na(Xc[j,]))) {
#                 Xc[j,] <- Centers[[mi.i]][[r]][j,]
#               }
#             }
#             Centers[[mi.i]][[r]] <- Xc
#           }
#         } else {
#           error[error$tau == tau + 1 & error$center == r, c("Cluster", "Regression")] <-
#             error[error$tau == tau & error$center == r, c("Cluster", "Regression")]
#         }
#       }
#       # update the counter:
#       tau <- tau + 1
#     }
#
#     mi.final.ojectives.values <- error[error$tau == tau - 1, ]
#
#     if(!is.null(save.path)){
#       save(error, error.mi, mi.final.ojectives.values,
#            file = paste0(save.path, "__Khorshidi.V1_all.Rdata"))
#     }
#     # Last select final partition set
#     optimal.final.obj <- mi.final.ojectives.values[mi.final.ojectives.values$Dominated == 0, ]
#   }
#
#   if (!update.all.IMversion) {
#     # V.Khorshidi.2: independent loops for each imputed dataset
#     mi.final.ojectives.values <- lapply(1:mi.m, function(mi.i) {
#       Z <- X[[mi.i]][, sel.col]
#       error <- data.frame(
#         'tau' = rep(1:termination.criteria, each = N),
#         'center' = rep(1:N, termination.criteria),
#         "Dominated" = NA )
#       tau <- 1 # iteration counter
#       terminate <- FALSE
#
#       while (!terminate) {
#         # check termination criteria:
#         terminate <- tau >= termination.criteria
#
#         if (tau == 1) {
#           set.1 <- 1:N
#         } else {
#           set.1 <- error[error$tau == tau - 1 & error$Dominated %in% c(1), "center"]
#         }
#         for (r in set.1) {
#           a <- Allocation_Distance(Z, Centers[[mi.i]][[r]])
#           Cl <- as.data.frame(a$Allocation)
#           colnames(Cl) <- paste("Cl", 1:ncol(Cl), sep = "")
#           assign(paste("MI", mi.i, ".Cl", r, sep = ""), Cl, pos = parent.env(environment()))
#           error[error$tau == tau & error$center == r, 'Cluster'] <- objective_clustering(a)
#
#           CVE <- tryCatch(CVE.fun(list(partition = data.frame(part = apply(Cl, 1, which.max)),
#                                        data = Y,
#                                        nfolds = nfolds)),
#                           error = function(e){
#                             CVE.fun(list(partition = data.frame(part = apply(Cl, 1, which.max)),
#                                          data = Y,
#                                          nfolds = nfolds))
#                           })
#           error[error$tau == tau & error$center == r, 'Regression'] <- CVE$value
#           error[error$tau == tau & error$center == r, 'Error'] <- CVE$error
#
#         }
#
#         # Find pareto non optimal solutions
#         error[error$tau == tau, ] <- pareto(error[error$tau == tau, ])
#         for (r in 1:N){
#
#           if (error[error$tau == tau & error$center == r, "Dominated"] > 0) {
#             Xc <- data.frame(matrix(0, nrow(Centers[[mi.i]][[r]]), p))
#             # for each center:
#             for (j in 1:nrow(Xc)) {
#               # update the coefficient:
#               Zq <- Z[get(paste("MI", mi.i, ".Cl", r, sep = ""))[, j] == 1,
#                       ][sample(nrow(Z[get(paste("MI", mi.i, ".Cl", r, sep = ""))[, j] == 1, ]), 1), ]
#               if (colSums(get(paste("MI", mi.i, ".Cl", r, sep = "")))[j] > 0) {
#                 Xc[j, ] <- Centers[[mi.i]][[r]][j,] +
#                   eta / ((1 + colSums(get(paste("MI", mi.i, ".Cl", r, sep = "")))[j]) ^ 0.75) *
#                   (Zq - Centers[[mi.i]][[r]][j,]) / sum(abs(Zq - Centers[[mi.i]][[r]][j,]))
#               } else{
#                 Xc[j,] <- Centers[[mi.i]][[r]][j,]
#               }
#               if (any(is.na(Xc[j,]))) {
#                 Xc[j,] <- Centers[[mi.i]][[r]][j,]
#               }
#             }
#             Centers[[mi.i]][[r]] <- Xc
#           } else {
#             error[error$tau == tau + 1 & error$center == r, c('Cluster', 'Regression', 'Error')] <-
#               error[error$tau == tau & error$center == r, c('Cluster', 'Regression', 'Error')]
#           }
#         }
#         # update the counter:
#         tau <- tau + 1
#
#       }
#
#       if(!is.null(save.path)){
#         save(error, file = paste0(save.path, "__Khorshidi.V2_mi.", mi.i, ".Rdata"))
#       }
#
#       error[error$tau == tau - 1, ]
#     })
#     # Then combine objective across imputation
#     final.obj <- data.frame(t(sapply(1:N, function(r){
#       c(r, rowMeans(data.frame(sapply(1:mi.m, function(mi.i){
#         as.numeric(
#           mi.final.ojectives.values[[mi.i]][r, c("Cluster", "Regression")])
#       }))))
#     })))
#     colnames(final.obj) <- c("center", "Cluster", "Regression")
#     # Last select final partition set
#     final.obj.dom <- pareto(final.obj)
#
#     if(!is.null(save.path)){
#       save(final.obj.dom,
#            file = paste0(save.path, "__Khorshidi.V2_finalCombined.Rdata"))
#     }
#
#     optimal.final.obj <- final.obj.dom[final.obj.dom$Dominated == 0, ]
#   }
#
#   # If several optimal, find the ideal
#   if(nrow(optimal.final.obj)>1){
#     optimal.final.obj.sc <- data.frame(scale(optimal.final.obj[, c("Cluster", "Regression")]))
#     optimal.final.obj.sc <- apply(optimal.final.obj.sc, 2, function(x){
#       if(any(!is.finite(x))){rep(0, length(x))}else{x}
#     })
#     ideal.optimal <- apply(optimal.final.obj.sc, 2, min)
#     similarities <- apply(optimal.final.obj.sc, 1, function(x){
#       (x %*% ideal.optimal) / (
#         t(x) %*% x * t(ideal.optimal) %*% ideal.optimal
#       )
#     })
#     optimal.final.obj <- optimal.final.obj[which.max(similarities), ]
#   }
#   # And apply consensus to the set to get 1 final partition
#   Selected.r <- optimal.final.obj[1, "center"]
#   binded.part <- data.frame(do.call(cbind, lapply(1:mi.m, function(mi.i){
#     as.character(apply(get(paste("MI", mi.i, ".Cl", Selected.r, sep = "")), 1, function(x){which.max(x)}))
#   })))
#   if(mi.m>1){
#     concensus.part <- MultiCons(DB = binded.part, Clust_entry = T, Plot = F)$Partitions
#   } else {
#     concensus.part <- unname(unlist(binded.part))
#   }
#   return(concensus.part)
# }
#
# # **   Use version in COVID; Is more complete
# # Consensus.UnSup_clustring  (renamed unsupMI)
#
# # **   Use version in COVID; Is more complete
# # PartitionGeneration
#
# # **   Use version in COVID; Is more complete
# # MIClust.mpool
#
# # **
# PerformancesEvaluation <- function(partition, true.unsup, true.sup, true.latent,
#                                    data.surv, TMIN = 2, TMAX = 5){
#   # Evaluate partitions (number of clusters, ARI, AUC diff with true Sup/unsup/latent
#   #      partition)
#   # Returns a list of named performances values
#
#   # partition: vector containing cluster ids (after removing small clusters)
#   # true.unsup: true partition UNSUP (ie data structure) for comparison (vector)
#   # true.sup: true partition SUP (ie using survivial parameters) for comparison (vector)
#   # true.latent: true partition (LATENT) (ie combining both) for comparison (vector)
#   # data.surv: dataframe with variables time and status
#   # TMAX: timepoint to analyze AUC
#   # TMIN: timepoint to stat analyzing AUC
#
#   data <- data.frame(data.surv,
#                      Cluster = factor(partition),
#                      Unsup = true.unsup,
#                      Sup = true.sup,
#                      Latent = true.latent)
#   CC <- complete.cases(data$Cluster)
#   data <- data[CC, ]
#
#   # Estimated number of clusters
#   Nbclust <- length(levels(data$Cluster[drop = T]))
#
#   # Cluster strcture ####
#   ARI.Latent <- ARI(data$Cluster, data$Latent)
#   ARI.Unsup <- ARI(data$Cluster, data$Unsup)
#
#   # Association patition - survival data ####
#   if(nlevels(data$Cluster) == 1){data$Cluster <- 1}
#
#   data$LP <- suppressWarnings(
#     predict(survival::coxph(Surv(time, status) ~  Cluster, data), type = "lp"))
#   data$LP.latent <- suppressWarnings(
#     predict(survival::coxph(Surv(time, status) ~  Latent, data), type = "lp"))
#   data$LP.sup <- suppressWarnings(
#     predict(survival::coxph(Surv(time, status) ~  Sup, data), type = "lp"))
#   timeS <- seq(from = TMIN, to = TMAX, length.out = 100)[-1]
#
#   auc <- Extract.AUC(data = data, predictor = "LP", time.vect = timeS)
#   auc.latent <- Extract.AUC(data = data, predictor = "LP.latent", time.vect = timeS)
#   auc.sup <- Extract.AUC(data = data, predictor = "LP.sup", time.vect = timeS)
#
#   # out ####
#   ret <- list(Nbclust = Nbclust,
#               N.classified = sum(CC),
#
#               DeltaAUCLatent.Tmax = auc$AUC.Tmax - auc.latent$AUC.Tmax,
#               DeltaAUCLatent.med = auc$AUC.med - auc.latent$AUC.med,
#               DeltaAUCSup.Tmax = auc$AUC.Tmax - auc.sup$AUC.Tmax,
#               DeltaAUCSup.med = auc$AUC.med - auc.sup$AUC.med,
#
#               AUCLatent.Tmax = auc.latent$AUC.Tmax,
#               AUCLatent.med = auc.latent$AUC.med,
#               AUCSup.Tmax = auc.sup$AUC.Tmax,
#               AUCSup.med = auc.sup$AUC.med,
#
#               ARI.Latent = ARI.Latent,
#               ARI.Unsup = ARI.Unsup
#   )
#
#   return(ret)
#
# }
#
# # *** internal
# Extract.AUC <- function(data, predictor, time.vect){
#   # Wrapper to evaluate time dependent AUC
#   # Returns AUC at max(time.vect) and median AUC along time.vect
#
#   # data: dataframe containg time and staus inforamtion, as well as marker
#   # predictor: colname of the marker to use to calculate AUC
#   # time.vect: vector of timepoints to consider
#
#   AUCs <- timeROC(data$time, delta = data$status, marker = data[, predictor],
#                   cause = 1, times = time.vect)
#   return(list(AUC.Tmax = unname(AUCs$AUC[length(time.vect)]),
#               AUC.med = median(AUCs$AUC, na.rm = T)))
# }
#
# MImpute.surv.miceobj <- function(data, mi.m){
#   # Performs imputation of the missing data
#   #   using multiple imputation
#   # Returns mids object
#
#   # data: dataframe with incomplete data
#   # mi.m: number of imputaitons for multiple imputation methods
#
#   data$H0 <- mice::nelsonaalen(data, time, status)
#   tmp <- suppressWarnings(mice::mice(data, m = 1, maxit = 0, print = F))
#   my.method <- tmp$method
#   my.predictorMatrix <- tmp$predictorMatrix
#   my.method[c("time", "status", "H0")] <- ""
#   my.predictorMatrix[, colnames(my.predictorMatrix) %in% c("time")] <- 0
#   if(sum(sapply(data, function(x){any(is.na(x))})) > 1){ max.it <- 10 } else { max.it <- 1 }
#   set.seed(1)
#   imp <- mice::complete(mice::mice(data, m = mi.m, method = my.method,
#                                    predictorMatrix = my.predictorMatrix,
#                                    maxit = max.it, print = F), "long", include = T)
#   imp <- imp[, !colnames(imp) %in% c("H0")]
#   imp <- mice::as.mids(imp)
#
#   return(imp)
# }
#
# extract.data <- function(scenari, to.rm, to.add){
#   # Downloads the data for the coresponding scenario, with
#   #   specifeid columns removed and added
#
#   # scenari: name of the scneario folder to download
#   # to.rm: vector of caracters, columnes that contain at leat of the lements are
#   #        removed from the data
#   # to.add: named vector. The colnums will be added with name used for colname
#   #         and element as the data in the column
#
#   load(paste(Simu.Path, scenari, "AllResults.RData", sep = "/"))
#
#   aa <- unique(unlist(sapply(to.rm, function(x){grep(x, colnames(AllResults), fixed = T)})))
#   AllResults <- AllResults[, -aa]
#   for(i in 1:length(to.add)){
#     AllResults[, names(to.add)[i]] <- to.add[i]
#   }
#   return(AllResults)
# }
#
#
# TABHRcoxphGEN <- function(tab, varint, delvarint, vect.var, vect.quali, langue = "fr"){
#   # Wrapper function to arragne results from univaraible cox models
#
#   # tab: dataset
#   # varint: character string, name of event variable
#   # delvarint: character string, name of time variable
#   # vect.var: vector of character strings, name of explicative variables
#   # vect.var: vector of 1 and 0s, 1 indicating if variables in vect.var are qualitaive, 0 otherwise
#   # langue: language to display the results
#
#   if (langue %in% c("fr","FR","Fr","fR"))
#   {
#     langue="fr"
#   }
#   if (langue %in% c("en","EN","an","AN","An","En","eN","aN"))
#   {
#     langue="en"
#   }
#   if (!(langue %in% c("fr","an","en")))
#   {
#     cat("Vous avez choisi une langue que je ne connais pas !! \n Le français sera pris par défaut \n")
#     langue="fr"
#   }
#   noms<-names(tab)
#   i <- 1
#   variable <- vect.var[i]
#   quali <- vect.quali[i]
#   res <- TABHRcoxph(tab, varint, delvarint, variable, quali)
#
#   for (i in 2:length(vect.var))
#   {
#     variable <- vect.var[i]
#     quali <- vect.quali[i]
#     res <- rbind(res,TABHRcoxph(tab, varint, delvarint, variable, quali))
#   }
#   res <- data.frame(res)
#
#   if (langue=="fr")
#   {
#     names(res) <- c("Paramètres","Valeurs","N","HR","IC95%","pvalue")
#   }
#   if (langue=="en")
#   {
#     names(res) <- c("Parameters","Values","N","HR","IC95%","pvalue")
#   }
#   return(res)
#
# }
#
#
# TABHRcoxph <- function(tab, varint, delvarint, variable, quali) {
#   # applies univaraible cox model and display results
#
#   # tab: dataset
#   # varint: character string, name of event variable
#   # delvarint: character string, name of time variable
#   # variable: character strings, name of explicative variable
#   # quali, 1 if varaible is qualitative, 0 otherwise
#
#   noms <- names(tab)
#   ind  <- match(variable,noms)
#   ind2 <- match(varint,noms)
#   ind4 <- match(delvarint,noms)
#
#   if (quali==1)
#   {
#     if (nlevels(as.factor(tab[,ind]))==2 )
#     {
#       res<-coxph(Surv(tab[,ind4],tab[,ind2])~as.factor(tab[,ind]))
#
#       HR <-   round(exp(res$coefficients),2)
#       lower <- round(exp(res$coefficients-1.96*sqrt(diag(res$var))),2)
#       upper <- round(exp(res$coefficients+1.96*sqrt(diag(res$var))),2)
#       pvalue <- format.pv(1-pchisq((res$coefficients/sqrt(diag(res$var)))**2,df=1))
#       resultat<- cbind("",HR,paste0("[",lower," - ",upper,"]"),pvalue)
#
#       resultat <- rbind(c(variable,"1.00","",""),resultat)
#
#       tableau <- as.data.frame(table (tab[,ind]))
#
#       resultat <- cbind(resultat[,1],levels(tableau$Var1),resultat[,2:4])
#       nbech <- length(tab[,ind2][is.na(tab[,ind2])==F])
#       resultat2 <- cbind(resultat[,1:2],tableau[,-1],resultat[,-c(1,2)])
#       return(resultat2)
#     } else
#     {
#       res<-coxph(Surv(tab[,ind4],tab[,ind2])~as.factor(tab[,ind]))
#       or <-   round(exp(res$coefficients),2)
#       lower <- round(exp(res$coefficients-1.96*sqrt(diag(res$var))),2)
#       upper <- round(exp(res$coefficients+1.96*sqrt(diag(res$var))),2)
#       pvalue <- sapply(1-pchisq((res$coefficients/sqrt(diag(res$var)))**2,df=1), format.pv)
#       resultat<- cbind("",or,paste0("[",lower," - ",upper,"]"),pvalue)
#
#       resultat <- rbind(c(variable,"1.00","",""),resultat)
#       tableau <- as.data.frame(table (tab[,ind]))
#
#       resultat <- cbind(resultat[,1],levels(as.factor(tab[,ind])),resultat[,2:4])
#       nbech <- length(tab[,ind2][is.na(tab[,ind2])==F])
#       nbval <- matrix(NA,ncol=nlevels(as.factor(tab[,ind])),nrow=1)
#       for (i in 1:nlevels(as.factor(tab[,ind])))
#       {
#         for (j in 1:nlevels(as.factor(tab[,ind])))
#         {
#           if (tableau$Var1[j]==levels(as.factor(tab[,ind]))[i]) { nbval[1,i] <- tableau$Freq[j]}
#         }
#       }
#       nbval <- as.vector(nbval)
#       resultat2 <- cbind(resultat[,1:2],nbval,resultat[,-c(1,2)])
#       return(resultat2)
#     }
#   }
#
#   if (quali==0)
#   {
#     res<-coxph(Surv(tab[,ind4],tab[,ind2])~tab[,ind])
#
#     or <-   round(exp(res$coefficients),2)
#     lower <- round(exp(res$coefficients-1.96*sqrt(res$var)),2)
#     upper <- round(exp(res$coefficients+1.96*sqrt(res$var)),2)
#     pvalue <- sapply(1-pchisq((res$coefficients/sqrt(res$var))**2,df=1), format.pv)
#     resultat<- cbind(or,paste0("[",lower," - ",upper,"]"),pvalue)
#
#     resultat <- c(variable,resultat)
#     nbech <- length(tab[,ind][is.na(tab[,ind])==F])
#     resultat3 <- c(resultat[1],'',nbech,resultat[2:4])
#     return(resultat3)
#   }
# }
#
# format.pv <- function(p, text = F) {
#   # Format fucntion for p-values
#
#   # p: pvalue to format
#   # text: T to display "p=", F to display only the value
#
#   if(p < 0.0001) return("<0.0001")
#   if(p >= 0.0001 & p < 0.00095) ifelse(text==F,return(sprintf("%.4f", p)),return(paste("=",sprintf("%.4f", p),sep="")))
#   if(p >= 0.00095 & p <= 0.0095) ifelse(text==F,return(as.character(signif(p,1))),return(paste("=",as.character(signif(p,1)),sep="")))
#   if(p > 0.0095 & p < 0.0995) ifelse(text==F,return(sprintf("%.3f", signif(p,2))),return(paste("=",sprintf("%.3f", signif(p,2)),sep="")))
#   if(p >= 0.0995) ifelse(text==F,return(sprintf("%.2f", signif(p,2))),return(paste("=",sprintf("%.2f", signif(p,2)),sep="")))
# }
#
# format.hr <- function(z){
#   # Rounds hr value for table dislay
#
#   #z: hr value to format
#
#   if (z < 0.05) return(sprintf("%.3f", z))
#   if (z <= 9.95 & z >= 0.05) return(sprintf("%.2f", z))
#   if (z > 9.95) return(sprintf("%.1f", z))
# }
