# Simulation <- function(df.param, where,
#                        md.type = c("MCAR", "MAR", "MNAR", "MNARWK"),
#                        simu_Concentration = F,
#                        Calcul_Fluo = F,
#                        ampute_NA = F,
#                        Estim_Concentration = F,
#                        ampute_LOD = F,
#                        impute_NA.LOD = F,
#                        Partition_Generation = F,
#                        Update_Multicons_partitions = F,
#                        Eval_partitions = F,
#                        simu = "new",
#                        ResDoss = "Simulation") {
#   # df.param: dataframe of 1 observation containing the scenario parameters
#   #  as generated in Simulation_parameters.R
#   # where: path indicating parent floder to save results of the simulation
#   # ResDoss: path of grand-parent floder to save results of the simulation
#   # md.type:types of missing data mechanism to evaluate
#   # simu_Concentration ; Calcul_Fluo ; ampute_NA ; Estim_Concentration ;
#   # ampute_LOD ; impute_NA.LOD ; Partition_Generation ; Update_Multicons_partitions ;
#   # Eval_partitions ; Plot_res: set to T/F to perform (or not) the step
#   # simu : either "new" or a number for a specific simulation
#
#   # Intit ####
#   if (simu == "new") {
#
#     simu.list <- dir(paste(ResDoss, where, sep = "/"), pattern = "s[[:digit:]]")
#
#     if (length(simu.list) < 1){
#       # First simulation
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
#   set.seed(f)
#   RNG_save <- runif(9) * 10 ^ 9
#
#   formula <- list(
#     both = as.formula("log(Y) ~ log( a.low + (delta) / ((1 + (x / c.50) ^ gamma ) ^ asym.exp))"),
#     alow = as.formula("log(Y) ~ log( a.low + (delta) / (1 + (x / c.50) ^ gamma))"),
#     asym = as.formula("log(Y) ~ log((delta) / ((1 + (x / c.50) ^ gamma ) ^ asym.exp))"),
#     none = as.formula("log(Y) ~ log((delta) / (1 + (x / c.50) ^ gamma))"))
#
#   # Set parameters####
#   for (l in names(df.param)) {
#
#     assign(x = l, value = eval(parse(text = df.param[l])))
#
#   }
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
#       c("simu_Concentration", "Calcul_Fluo", "ampute_NA",
#         "Estim_Concentration", "ampute_LOD", "impute_NA.LOD",
#         "Partition_Generation", "Eval_partitions",
#         "Update_Multicons_partitions"),
#       c("Start"), sep = ".")
#   }
#
#   mudiff <- sapply(1:length(STD.c.per), function(i){
#
#     ppm <- sapply(means, function(tmp){
#       tmp[i]
#     })
#     Km <- which.min(ppm)
#     Kc <- ifelse(length(covs) == 1, 1, Km)
#
#     deno <- sum(k.prop[ppm == ppm[Km]])
#     pp <- STD.c.per[i] / deno
#     if (pp > 1){
#       ppm.2 <- ppm
#       ppm.2[Km] <- NA
#       Pmin <- deno
#       Km <- which.min(ppm.2)
#       Kc <- ifelse(length(covs) == 1, 1, Km)
#       deno <- sum(k.prop[ppm == ppm[Km]])
#       pp <- (STD.c.per[i] - Pmin) / deno
#     }
#
#     log(STD.c[1, i])  - qnorm(pp) * sqrt(covs[[Kc]][i, i]) - means[[Km]][i]
#
#   })
#
#   means <- lapply(1:length(means), function(kk){
#
#     means[[kk]] + mudiff
#
#   })
#
#   # 1 ####
#   if (simu_Concentration){
#
#     set.seed(ss[["simu_Concentration.Start"]])
#     if (verbose) cat("1. Generation of theoric concentration... \n")
#     tmp <- DataGeneration(n = n, means = means, covs = covs,
#                           lognormal = T, k.prop = k.prop)
#     db <- tmp[["db"]]
#     group <- tmp[["group"]]
#
#     save(db, file = paste(Dossier, "1_data_ExactConcentration(simulated).Rdata",
#                           sep = "/"))
#     save(group, file = paste(Dossier, "1_data_Groups.Rdata", sep = "/"))
#     ss[["simu_Concentration.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 2 ####
#   if (Calcul_Fluo) {
#
#     set.seed (ss[["Calcul_Fluo.Start"]])
#     if (verbose) cat("2. Generation of fluorescence... \n")
#     if (!exists("db")) load(paste(Dossier,
#                                   "1_data_ExactConcentration(simulated).Rdata",
#                                   sep = "/"))
#
#     tmp <- FluoGeneration(data = db, curve.param = C.param.Generation,
#                           STD.c = STD.c, my.formula = formula)
#     db.fluo <- tmp[["data.fluo"]]
#     std <- tmp[["standards"]]
#
#     save(db.fluo, file = paste(Dossier, "2_data_fluo.Rdata", sep = "/"))
#     save(std, file = paste(Dossier, "2_standards_fluo.Rdata", sep = "/"))
#     ss[["Calcul_Fluo.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 3 ####
#   if (ampute_NA) {
#     set.seed(ss[["ampute_NA.Start"]])
#     if (verbose) cat("3. Data amputation... \n")
#     if (!exists("db.fluo")) load(paste(Dossier, "2_data_fluo.Rdata", sep = "/"))
#
#     if (md.prop == 0){
#
#       db.fluo.NAS <- list("no.na" = db.fluo)
#
#     } else {
#
#       # This ligne was rendered useless but is ketp to maintain the alea
#       fluo.z <- rnorm(n)
#
#       db.fluo.NAS <- lapply(md.type, function(md.type.i){
#         tt <- MissingDataGeneration(md.type = md.type.i,
#                                     data = db.fluo,
#                                     md.prop = md.prop,
#                                     bycases = bycases,
#                                     dep = MD.dep,
#                                     md.patt = md.patt,
#                                     md.patt.freq = md.patt.freq,
#                                     md.patt.weights = md.patt.weights,
#                                     log = T)
#
#         ss[[paste0("ampute_NA.", toupper(md.type.i), "End")]] <<- .Random.seed
#
#         return(tt)
#
#       })
#
#       names(db.fluo.NAS) <- tolower(md.type)
#
#     }
#
#     save(db.fluo.NAS,
#          file = paste(Dossier, "3_data_fluo_M.AR.Rdata", sep = "/"))
#     ss[["ampute_NA.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 4 ####
#   if (Estim_Concentration) {
#
#     set.seed(ss[["Estim_Concentration.Start"]])
#     if (verbose) cat("4. Generation of experimental concentration... \n")
#     if (!exists("db.fluo.NAS"))
#       load(paste(Dossier, "3_data_fluo_M.AR.Rdata", sep = "/"))
#     if (!exists("db.fluo"))
#       load(paste(Dossier, "2_data_fluo.Rdata", sep = "/"))
#     if (!exists("std"))
#       load(paste(Dossier, "2_standards_fluo.Rdata", sep = "/"))
#
#     tmp <- sapply(names(db.fluo.NAS), function(NA.t){
#
#       if (verbose) cat(paste0("...Missing Mechanism ", toupper(NA.t), "... \n"))
#
#       tt <- ConcentrationEstimation(data = db.fluo.NAS[[NA.t]],
#                                     standards = std,
#                                     curve.param = C.param,
#                                     STD.c = STD.c,
#                                     my.formula = formula)
#
#       who <- paste0("Estim_Concentration.", toupper(NA.t), "End")
#       ss[[who]] <<- .Random.seed
#
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     db.concS <- sapply(names(db.fluo.NAS), function(i){
#
#       tmp[[i]][["db.conc"]]
#
#     }, USE.NAMES = T, simplify = F)
#     std.concS <- sapply(names(db.fluo.NAS), function(i){
#
#       tmp[[i]][["std.C.est"]]
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(db.concS,
#          file = paste(Dossier,
#                       "4_data_concentration(Estimated)_M.AR.Rdata",
#                       sep = "/"))
#     save(std.concS,
#          file = paste(Dossier,
#                       "4_standards_concentration(Estimated)_M.AR.Rdata",
#                       sep = "/"))
#
#     if (verbose) cat("Generation of experimental concentration (complete)...\n")
#     if (!exists("db.fluo")) load(paste(Dossier, "2_data_fluo.Rdata", sep = "/"))
#
#     db.conc.complete <- ConcentrationEstimation(data = db.fluo,
#                                                 standards = std,
#                                                 curve.param = C.param,
#                                                 STD.c = STD.c,
#                                                 my.formula = formula,
#                                                 complete = T)
#
#     save(db.conc.complete,
#          file = paste(Dossier,
#                       "4_data_concentration(Estimated)_complete.Rdata",
#                       sep = "/"))
#     ss[["Estim_Concentration.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 5 ####
#   if (ampute_LOD) {
#
#     set.seed(ss[["ampute_LOD.Start"]])
#     if (verbose) cat("5. LOD amputation... \n")
#     if (!exists("db.concS"))
#       load(paste(Dossier, "4_data_concentration(Estimated)_M.AR.Rdata",
#                  sep = "/"))
#     if (!exists("std.concS"))
#       load(paste(Dossier, "4_standards_concentration(Estimated)_M.AR.Rdata",
#                  sep = "/"))
#
#     tmp <- sapply(names(db.concS), function(NA.t){
#
#       tt <- LODGeneration(data = db.concS[[NA.t]],
#                           LOD.v = std.concS[[NA.t]][1, ])
#
#       who <- paste0("ampute_LOD.", toupper(NA.t), "End")
#       ss[[who]] <<- .Random.seed
#
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     db.conc.lodS <- sapply(names(tmp), function(i){
#
#       tmp[[i]][["db.cens"]]
#
#     }, USE.NAMES = T, simplify = F)
#
#     db.I.lodS <- sapply(names(tmp), function(i){
#
#       tmp[[i]][["db.lod"]]
#
#     }, USE.NAMES = T, simplify = F)
#
#     rm(tmp)
#
#     save(db.conc.lodS,
#          file = paste(Dossier, "5_data_concentration(Estimated)_M.AR_LOD.Rdata",
#                       sep = "/"))
#     save(db.I.lodS,
#          file = paste(Dossier, "5_data_LOD_Indicators_M.AR.Rdata",
#                       sep = "/"))
#     ss[["ampute_LOD.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 6 ####
#   if (impute_NA.LOD){
#
#     set.seed(ss[["impute_NA.LOD.Start"]])
#     if (verbose) cat("6. Data imputation... \n")
#
#     if (!exists("db.conc.lodS"))
#       load(paste(Dossier, "5_data_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     if (!exists("db.I.lodS"))
#       load(paste(Dossier, "5_data_LOD_Indicators_M.AR.Rdata",
#                  sep = "/"))
#
#     if (!exists("db.concS"))
#       load(paste(Dossier, "4_data_concentration(Estimated)_M.AR.Rdata",
#                  sep = "/"))
#
#     if (!exists("std.concS"))
#       load(paste(Dossier, "4_standards_concentration(Estimated)_M.AR.Rdata",
#                  sep = "/"))
#
#     md.care.short <- gsub(".ch", "", md.care, fixed = T)
#     md.care.short <- gsub(".CritCF", "", md.care.short, fixed = T)
#     md.care.short <- gsub(".Basagana", "", md.care.short, fixed = T)
#     md.care.short <- gsub(".MultiCons", "", md.care.short, fixed = T)
#     md.care.short <- gsub(".Bruckers", "", md.care.short, fixed = T)
#     md.care.short <- gsub("mi.", "", md.care.short, fixed = T)
#     md.care.short <- unique(gsub("si", "mi", md.care.short))
#
#     db.imputedS <- sapply(names(db.conc.lodS), function(NA.t){
#
#       if (verbose) cat(paste0("...Missing Mechanism ", NA.t, "...\n"))
#
#       tmp.imputed <- sapply(md.care.short, function(md.care.i){
#
#         tt <- MyImputer(md.care.i = md.care.i,
#                         data = db.conc.lodS[[NA.t]],
#                         data.lod = db.I.lodS[[NA.t]],
#                         data.no.censd = db.concS[[NA.t]],
#                         standards = std.concS[[NA.t]],
#                         mi.m = mi.m)
#
#         who <- paste0("impute_NA.LOD.", toupper(NA.t), md.care.i, "End")
#         ss[[who]] <<- .Random.seed
#
#         return(tt)
#
#       }, USE.NAMES = T, simplify = F)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(db.imputedS,
#          file = paste(Dossier,
#                       "6_data_concentration(Estimated)_M.AR_LOD_IMPUTED.Rdata",
#                       sep = "/"))
#     ss[["impute_NA.LOD.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 7 ####
#   if (Partition_Generation){
#
#     set.seed(ss[["Partition_Generation.Start"]])
#     if (verbose) cat("7. Partition Generation... \n")
#     if (!exists("db.conc.lodS"))
#       load(paste(Dossier, "5_data_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     if (!exists("db.imputedS"))
#       load(paste(Dossier,
#                  "6_data_concentration(Estimated)_M.AR_LOD_IMPUTED.Rdata",
#                  sep = "/"))
#
#     if (!exists("db.conc.complete"))
#       load(paste(Dossier, "4_data_concentration(Estimated)_complete.Rdata",
#                  sep = "/"))
#
#     db.partitionS <- sapply(names(db.imputedS), function(NA.t){
#
#       if (verbose) cat(paste0("...Missing Mechanism ", NA.t, "... \n"))
#
#       tmp.partitions <- sapply(md.care, function(method){
#
#         if (verbose) cat(paste(".....Missing data method =", method, "... \n"))
#         method.data <- gsub(".ch", "", method, fixed = T)
#         method.data <- gsub(".CritCF", "", method.data, fixed = T)
#         method.data <- gsub(".Basagana", "", method.data, fixed = T)
#         method.data <- gsub(".MultiCons", "", method.data, fixed = T)
#         method.data <- gsub(".Bruckers", "", method.data, fixed = T)
#         method.data <- gsub("mi.", "", method.data, fixed = T)
#         method.data <- gsub("si", "mi", method.data, fixed = T)
#
#         tt <- GetPartition(data = db.imputedS[[NA.t]][[method.data]],
#                            data.raw =  db.conc.lodS[[NA.t]],
#                            md.care = method,
#                            clust.algo = clust.algo)
#
#         who <- paste0("Partition_Generation.", toupper(NA.t), method, "End")
#         ss[[who]] <<- .Random.seed
#
#         return(tt)
#
#       }, USE.NAMES = T, simplify = F)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(db.partitionS,
#          file = paste(Dossier,
#                       "7_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                       sep = "/"))
#
#     if (verbose) cat("Complete Partition Generation (ref)... \n")
#     if (!exists("db.conc.complete"))
#       load(paste(Dossier, "4_data_concentration(Estimated)_complete.Rdata",
#                  sep = "/"))
#
#     complete.db.partitions.ch <- GetPartition(data = list(db.conc.complete),
#                                               md.care = "cc.ch",
#                                               clust.algo = clust.algo)
#
#     save(complete.db.partitions.ch,
#          file = paste(Dossier,
#                       "7_partitions_concentration(Estimated)_complete.Rdata",
#                       sep = "/"))
#     ss[["Partition_Generation.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#
#   }
#
#   # 7bis ####
#   if (Update_Multicons_partitions){
#     if (verbose) cat("7bis. Multicons Partition Update... \n")
#     if (!exists("db.partitionS"))
#       load(paste(Dossier,
#                  "7_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     db.partitionS_n5 <- sapply(names(db.partitionS), function(NA.t, Limite = 5){
#       sapply(names(db.partitionS[[NA.t]]), function(method){
#         if(length(grep("MultiCons", method))>0){
#           for(i in 1:ncol(db.partitionS[[NA.t]][[method]])){
#             tp <- summary(factor(db.partitionS[[NA.t]][[method]][, i]),  maxsum = 500)
#             to.rm <- names(tp[which(tp < Limite)])
#             db.partitionS[[NA.t]][[method]][is.element(db.partitionS[[NA.t]][[method]][, i], to.rm), i] <- NA
#             db.partitionS[[NA.t]][[method]][, i] <- db.partitionS[[NA.t]][[method]][, i][drop = T]
#           }
#           db.partitionS[[NA.t]][[method]]
#
#         } else {
#           db.partitionS[[NA.t]][[method]]
#         }
#
#       }, USE.NAMES = T, simplify = F)
#     }, USE.NAMES = T, simplify = F)
#
#     db.partitionS_n10 <- sapply(names(db.partitionS), function(NA.t, Limite = 10){
#       sapply(names(db.partitionS[[NA.t]]), function(method){
#         if(length(grep("MultiCons", method))>0){
#           for(i in 1:ncol(db.partitionS[[NA.t]][[method]])){
#             tp <- summary(factor(db.partitionS[[NA.t]][[method]][, i]),  maxsum = 500)
#             to.rm <- names(tp[which(tp < Limite)])
#             db.partitionS[[NA.t]][[method]][is.element(db.partitionS[[NA.t]][[method]][, i], to.rm), i] <- NA
#             db.partitionS[[NA.t]][[method]][, i] <- db.partitionS[[NA.t]][[method]][, i][drop = T]
#           }
#           db.partitionS[[NA.t]][[method]]
#
#         } else {
#           db.partitionS[[NA.t]][[method]]
#         }
#
#       }, USE.NAMES = T, simplify = F)
#     }, USE.NAMES = T, simplify = F)
#
#     db.partitionS_n25 <- sapply(names(db.partitionS), function(NA.t, Limite = 25){
#       sapply(names(db.partitionS[[NA.t]]), function(method){
#         if(length(grep("MultiCons", method))>0){
#           for(i in 1:ncol(db.partitionS[[NA.t]][[method]])){
#             tp <- summary(factor(db.partitionS[[NA.t]][[method]][, i]),  maxsum = 500)
#             to.rm <- names(tp[which(tp < Limite)])
#             db.partitionS[[NA.t]][[method]][is.element(db.partitionS[[NA.t]][[method]][, i], to.rm), i] <- NA
#             db.partitionS[[NA.t]][[method]][, i] <- db.partitionS[[NA.t]][[method]][, i][drop = T]
#           }
#           db.partitionS[[NA.t]][[method]]
#
#         } else {
#           db.partitionS[[NA.t]][[method]]
#         }
#
#       }, USE.NAMES = T, simplify = F)
#     }, USE.NAMES = T, simplify = F)
#
#     db.partitionS_n50 <- sapply(names(db.partitionS), function(NA.t, Limite = 50){
#       sapply(names(db.partitionS[[NA.t]]), function(method){
#         if(length(grep("MultiCons", method))>0){
#           for(i in 1:ncol(db.partitionS[[NA.t]][[method]])){
#             tp <- summary(factor(db.partitionS[[NA.t]][[method]][, i]),  maxsum = 500)
#             to.rm <- names(tp[which(tp < Limite)])
#             db.partitionS[[NA.t]][[method]][is.element(db.partitionS[[NA.t]][[method]][, i], to.rm), i] <- NA
#             db.partitionS[[NA.t]][[method]][, i] <- db.partitionS[[NA.t]][[method]][, i][drop = T]
#           }
#           db.partitionS[[NA.t]][[method]]
#
#         } else {
#           db.partitionS[[NA.t]][[method]]
#         }
#
#       }, USE.NAMES = T, simplify = F)
#     }, USE.NAMES = T, simplify = F)
#
#     save(db.partitionS_n5,
#          file = paste(Dossier,
#                       "7bis(5)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                       sep = "/"))
#     save(db.partitionS_n10,
#          file = paste(Dossier,
#                       "7bis(10)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                       sep = "/"))
#     save(db.partitionS_n25,
#          file = paste(Dossier,
#                       "7bis(25)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                       sep = "/"))
#     save(db.partitionS_n50,
#          file = paste(Dossier,
#                       "7bis(50)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                       sep = "/"))
#   }
#
#   # 8 ####
#   if (Eval_partitions){
#     set.seed(ss[["Eval_partitions.Start"]])
#     if (verbose) cat("8. Partition Evaluation... \n")
#     if (!exists("db.fluo.NAS"))
#       load(paste(Dossier, "3_data_fluo_M.AR.Rdata", sep = "/"))
#
#     if (!exists("db.conc.lodS"))
#       load(paste(Dossier, "5_data_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     if (!exists("complete.db.partitions.ch"))
#       load(paste(Dossier,
#                  "7_partitions_concentration(Estimated)_complete.Rdata",
#                  sep = "/"))
#
#     if (!exists("group"))
#       load(paste(Dossier, "1_data_Groups.Rdata", sep = "/"))
#
#     # n0 ####
#     if (!exists("db.partitionS"))
#       load(paste(Dossier,
#                  "7_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     part.qualS <- sapply(names(db.partitionS), function(NA.t){
#
#       tt <- PartitionEvaluation(
#         data.part = db.partitionS[[NA.t]],
#         is.missing = is.na(db.partitionS[[NA.t]][["cc.ch"]]),
#         is.cens = db.conc.lodS[[NA.t]],
#         part.ref = complete.db.partitions.ch,
#         true.part = group)
#
#
#       who <- paste0("Eval_partitions.", toupper(NA.t), "End")
#       ss[[who]] <<- .Random.seed
#
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(part.qualS,
#          file = paste(
#            Dossier,
#            "8_eval_partitions_concentration(estimated)_M.AR_LOD.Rdata",
#            sep = "/"))
#
#     # n5 ####
#     if (!exists("db.partitionS_n5"))
#       load(paste(Dossier,
#                  "7bis(5)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     part.qualS_n5 <- sapply(names(db.partitionS_n5), function(NA.t){
#
#       tt <- PartitionEvaluation(
#         data.part = db.partitionS_n5[[NA.t]],
#         is.missing = is.na(db.partitionS_n5[[NA.t]][["cc.ch"]]),
#         is.cens = db.conc.lodS[[NA.t]],
#         part.ref = complete.db.partitions.ch,
#         true.part = group)
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(part.qualS_n5,
#          file = paste(
#            Dossier,
#            "8bis(5)_eval_partitions_concentration(estimated)_M.AR_LOD.Rdata",
#            sep = "/"))
#
#     # n10 ####
#     if (!exists("db.partitionS_n10"))
#       load(paste(Dossier,
#                  "7bis(10)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     part.qualS_n10 <- sapply(names(db.partitionS_n10), function(NA.t){
#
#       tt <- PartitionEvaluation(
#         data.part = db.partitionS_n10[[NA.t]],
#         is.missing = is.na(db.partitionS_n10[[NA.t]][["cc.ch"]]),
#         is.cens = db.conc.lodS[[NA.t]],
#         part.ref = complete.db.partitions.ch,
#         true.part = group)
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(part.qualS_n10,
#          file = paste(
#            Dossier,
#            "8bis(10)_eval_partitions_concentration(estimated)_M.AR_LOD.Rdata",
#            sep = "/"))
#
#     # n25 ####
#     if (!exists("db.partitionS_n25"))
#       load(paste(Dossier,
#                  "7bis(25)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     part.qualS_n25 <- sapply(names(db.partitionS_n25), function(NA.t){
#
#       tt <- PartitionEvaluation(
#         data.part = db.partitionS_n25[[NA.t]],
#         is.missing = is.na(db.partitionS_n25[[NA.t]][["cc.ch"]]),
#         is.cens = db.conc.lodS[[NA.t]],
#         part.ref = complete.db.partitions.ch,
#         true.part = group)
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(part.qualS_n25,
#          file = paste(
#            Dossier,
#            "8bis(25)_eval_partitions_concentration(estimated)_M.AR_LOD.Rdata",
#            sep = "/"))
#
#     # n50 ####
#     if (!exists("db.partitionS_n50"))
#       load(paste(Dossier,
#                  "7bis(50)_partitions_concentration(Estimated)_M.AR_LOD.Rdata",
#                  sep = "/"))
#
#     part.qualS_n50 <- sapply(names(db.partitionS_n50), function(NA.t){
#
#       tt <- PartitionEvaluation(
#         data.part = db.partitionS_n50[[NA.t]],
#         is.missing = is.na(db.partitionS_n50[[NA.t]][["cc.ch"]]),
#         is.cens = db.conc.lodS[[NA.t]],
#         part.ref = complete.db.partitions.ch,
#         true.part = group)
#       return(tt)
#
#     }, USE.NAMES = T, simplify = F)
#
#     save(part.qualS_n50,
#          file = paste(
#            Dossier,
#            "8bis(50)_eval_partitions_concentration(estimated)_M.AR_LOD.Rdata",
#            sep = "/"))
#
#     #seed ####
#     ss[["Eval_partitions.End"]] <- .Random.seed
#     save(ss, file = paste(Dossier, "Seeds.Rdata", sep = "/"))
#   }
#
#   return(NULL)
#
# }



PartitionEvaluation <- function(data.part, is.missing, is.cens,
                                part.ref, true.part){
  # Evaluate partitions

  # data.part: list of dataframes of partions (1 column per clustering
  #    algorithm). each dataframe correspond to a clustering method
  # is.missing: boolean vector identifying observations with
  #    missing data (T), from those without (F)
  # is.cens: dataframe before imputation with NA for missing and
  #    left-censored data.
  # part.ref: reference partition (ie partition on complete data)
  # true.part: true partition for comparison

  algo <- colnames(data.part[[1]])
  md.meth <- names(data.part)

  CC.data <- complete.cases(is.cens)
  CENS.data <- !CC.data & !is.missing
  NA.data <- is.missing & complete.cases(is.cens[, 2:3])
  NACENS.data <- is.missing & !complete.cases(is.cens[, 2:3])

  ARI <- ARI.cc <- ARI.nona <- ARI.nocens <- Nbclust <-
    Per.Inclass.cc <- Per.Inclass.na <- Per.Inclass.cens <-
    Per.Inclass.nacens <- ARI.full <- Nbclust.full <-
    Per.Inclass.full <- NULL

  for (i in md.meth) {
    for (j in algo) {
      temp.clust <- data.part[[i]][, j]
      CC <- complete.cases(temp.clust)

      if (length(temp.clust[CC]) > 0) {
        ARI <- c(ARI, ARI(temp.clust[CC], true.part[CC]))
        ARI.cc <- c(ARI.cc, ARI(temp.clust[CC.data & CC],
                                true.part[CC.data & CC]))
        ARI.nona <- c(ARI.nona, ARI(temp.clust[(CC.data | CENS.data) & CC],
                                    true.part[(CC.data | CENS.data) & CC]))
        ARI.nocens <- c(ARI.nocens, ARI(temp.clust[(CC.data | NA.data) & CC],
                                        true.part[(CC.data | NA.data) & CC]))
        Nbclust <- c(Nbclust, length(levels(factor(temp.clust))))
      } else {
        ARI <- c(ARI, NA)
        ARI.cc <- c(ARI.cc, NA)
        ARI.nona <- c(ARI.nona, NA)
        ARI.nocens <- c(ARI.nocens, NA)
        Nbclust <- c(Nbclust, 1)
      }
      Per.Inclass.cc <- c(Per.Inclass.cc,
                          sum(!CC & CC.data) / length(CC.data))
      Per.Inclass.na <- c(Per.Inclass.na,
                          sum(!CC & NA.data) / length(NA.data))
      Per.Inclass.cens <- c(Per.Inclass.cens,
                            sum(!CC & CENS.data) / length(CENS.data))
      Per.Inclass.nacens <- c(Per.Inclass.nacens,
                              sum(!CC & NACENS.data) / length(NACENS.data))
    }
  }

  for (j in algo) {
    complete.temp.clust <- part.ref[, j]
    if (sum(!is.na(complete.temp.clust)) == 0) {
      ARI.full <- c(ARI.full, NA)
      Nbclust.full <- c(Nbclust.full, NA)
      Per.Inclass.full <- c(Per.Inclass.full, NA)
    } else {
      ARI.full <- c(ARI.full, ARI(complete.temp.clust, true.part))
      Nbclust.full <- c(Nbclust.full,
                        length(levels(factor(complete.temp.clust))))
      Per.Inclass.full <- c(Per.Inclass.full, 0)
    }
  }

  names(ARI) <- names(ARI.cc) <- names(ARI.nona) <- names(ARI.nocens) <-
    names(Per.Inclass.cc) <- names(Per.Inclass.cens) <-
    names(Per.Inclass.na) <- names(Per.Inclass.nacens) <- names(Nbclust) <-
    paste(rep(md.meth, each = length(algo)), algo, sep = "_")
  names(ARI.full) <- names(Nbclust.full) <- names(Per.Inclass.full) <-
    paste("CompleteData.ch", algo, sep = "_")

  return(list(ARI = c(ARI, ARI.full),
              ARI.cc = c(ARI.cc, ARI.full),
              ARI.nona = c(ARI.nona, ARI.full),
              ARI.nocens = c(ARI.nocens, ARI.full),
              Nbclust = c(Nbclust, Nbclust.full),
              Per.Inclass.cc = c(Per.Inclass.cc, Per.Inclass.full),
              Per.Inclass.na = c(Per.Inclass.na, Per.Inclass.full),
              Per.Inclass.cens = c(Per.Inclass.cens, Per.Inclass.full),
              Per.Inclass.nacens = c(Per.Inclass.nacens, Per.Inclass.full)
  ))

}

Extract.centers <- function(df.param, where, simu,
                            ResDoss = "Simulation") {
  # df.param: dataframe of 1 observation containing the scenario parameters
  #  as generated in Simulation_parameters.R
  # where: path indicating parent floder to save results of the simulation
  # ResDoss: path of grand-parent floder to save results of the simulation
  # simu: number of the specific simulation

  # Init ####
  set.seed(simu)
  RNG_save <- runif(9) * 10 ^ 9

  formula <- list(
    both = as.formula("log(Y) ~ log( a.low + (delta) / ((1 + (x / c.50) ^ gamma ) ^ asym.exp))"),
    alow = as.formula("log(Y) ~ log( a.low + (delta) / (1 + (x / c.50) ^ gamma))"),
    asym = as.formula("log(Y) ~ log((delta) / ((1 + (x / c.50) ^ gamma ) ^ asym.exp))"),
    none = as.formula("log(Y) ~ log((delta) / (1 + (x / c.50) ^ gamma))"))
  Dossier <- paste(ResDoss, where, paste("s", simu, sep = ""), sep = "/")

  # Set parameters####
  for(l in names(df.param)) {
    assign(x = l, value = eval(parse(text = df.param[l])))
  }

  if(!exists("verbose")) verbose <- F

  mudiff <- sapply(1:length(STD.c.per), function(i){

    ppm <- sapply(means, function(tmp){
      tmp[i]
    })
    Km <- which.min(ppm)
    Kc <- ifelse(length(covs) == 1, 1, Km)

    deno <- sum(k.prop[ppm == ppm[Km]])
    pp <- STD.c.per[i] / deno
    if (pp > 1){
      ppm.2 <- ppm
      ppm.2[Km] <- NA
      Pmin <- deno
      Km <- which.min(ppm.2)
      Kc <- ifelse(length(covs) == 1, 1, Km)
      deno <- sum(k.prop[ppm == ppm[Km]])
      pp <- (STD.c.per[i] - Pmin) / deno
    }

    log(STD.c[1, i])  - qnorm(pp) * sqrt(covs[[Kc]][i, i]) - means[[Km]][i]

  })

  means <- lapply(1:length(means), function(kk){

    means[[kk]] + mudiff

  })

  # load ####
  load(paste(Dossier,
             "6_data_concentration(Estimated)_M.AR_LOD_IMPUTED.Rdata",
             sep = "/"))
  load(paste(Dossier,
             "4_data_concentration(Estimated)_complete.Rdata",
             sep = "/"))
  load(paste(Dossier,"1_data_Groups.Rdata", sep = "/"))
  # Extract centers ####
  set.seed(RNG_save[6])

  if(verbose) cat("6. Data imputation... \n")
  md.care.short <- gsub(".ch", "", md.care, fixed = T)
  md.care.short <- gsub(".CritCF", "", md.care.short, fixed = T)
  md.care.short <- gsub(".Basagana", "", md.care.short, fixed = T)
  md.care.short <- gsub(".MultiCons", "", md.care.short, fixed = T)
  md.care.short <- gsub(".Bruckers", "", md.care.short, fixed = T)
  md.care.short <- gsub("mi.", "", md.care.short, fixed = T)
  md.care.short <- unique(gsub("si", "mi", md.care.short))

  db.res.centers <- sapply(names(db.imputedS), function(NA.t){

    if(verbose)
      cat(paste0("...Missing Mechanism ", NA.t, "...\n"))

    sapply(md.care.short, MyCenters.extracteur,
           data = db.imputedS[[NA.t]],
           group = group,
           USE.NAMES = T, simplify = F)

  },
  USE.NAMES = T, simplify = F)

  db.res.centers <- c(
    db.res.centers,
    CompleteData = list(
      list(Nothing = MyCenters.extracteur(md.care.i = "CompleteData",
                                          data = db.conc.complete,
                                          group = group))
    ))

  return(db.res.centers)

}

MyCenters.extracteur <- function(md.care.i, data, group){
  # Returns center and covaraince of each group after the
  # different imputation methods
  # md.care.i: method of imputaiton (mi, std.curve, lod.5 or cc)
  # data: list with one element named md.care.i containg
  #    the imputed dataset(s) by the md.care.i method
  # group: true partition of the data (vector)

  if(md.care.i %in% c("mi", "std.curve", "lod.5")) {
    df <- data[[md.care.i]]
    CC <- rep(T, nrow(df[[1]]))
  }

  if(md.care.i == "cc") {
    df <- list(data[[md.care.i]][[1]])
    CC <- data[[md.care.i]][[2]]
  }

  if(md.care.i == "CompleteData") {
    df <- list(data)
    CC <- rep(T, nrow(df[[1]]))
  }

  ret <- data.frame(do.call(
    rbind,
    sapply(1:length(df), function(m){
      dtemp <- log(df[[m]])
      G1.SIGMA <- cov(dtemp[group[CC] == 1, c("X1", "X2", "X3")])
      G2.SIGMA <- cov(dtemp[group[CC] == 2, c("X1", "X2", "X3")])
      G3.SIGMA <- cov(dtemp[group[CC] == 3, c("X1", "X2", "X3")])

      data.frame(
        G1.X1.mean = mean(dtemp[group[CC] == 1, "X1"]),
        G1.X2.mean = mean(dtemp[group[CC] == 1, "X2"]),
        G1.X3.mean = mean(dtemp[group[CC] == 1, "X3"]),
        G1.X1X1.sigma = G1.SIGMA[1,1],
        G1.X1X2.sigma = G1.SIGMA[1,2],
        G1.X1X3.sigma = G1.SIGMA[1,3],
        G1.X2X2.sigma = G1.SIGMA[2,2],
        G1.X2X3.sigma = G1.SIGMA[2,3],
        G1.X3X3.sigma = G1.SIGMA[3,3],

        G2.X1.mean = mean(dtemp[group[CC] == 2, "X1"]),
        G2.X2.mean = mean(dtemp[group[CC] == 2, "X2"]),
        G2.X3.mean = mean(dtemp[group[CC] == 2, "X3"]),
        G2.X1X1.sigma = G2.SIGMA[1,1],
        G2.X1X2.sigma = G2.SIGMA[1,2],
        G2.X1X3.sigma = G2.SIGMA[1,3],
        G2.X2X2.sigma = G2.SIGMA[2,2],
        G2.X2X3.sigma = G2.SIGMA[2,3],
        G2.X3X3.sigma = G2.SIGMA[3,3],

        G3.X1.mean = mean(dtemp[group[CC] == 3, "X1"]),
        G3.X2.mean = mean(dtemp[group[CC] == 3, "X2"]),
        G3.X3.mean = mean(dtemp[group[CC] == 3, "X3"]),
        G3.X1X1.sigma = G3.SIGMA[1,1],
        G3.X1X2.sigma = G3.SIGMA[1,2],
        G3.X1X3.sigma = G3.SIGMA[1,3],
        G3.X2X2.sigma = G3.SIGMA[2,2],
        G3.X2X3.sigma = G3.SIGMA[2,3],
        G3.X3X3.sigma = G3.SIGMA[3,3]
      )
    },
    simplify = F)))

  return(ret)

}

bind_scenarios <- function(sc.list, what, who = "n0", path = "Simulation"){
  # Row binds results of scenarios in sc_list
  # sc_list: vector of file directory containeing simulation
  #    results
  # what: "left" for left-censored data only; "Missing" otherwise
  #    impacts the relabeling of the name of the methods
  # who:  "n0", or "n5" , "n10", "n25", "n50", for datasets with
  #    multicons threshold at 0, 5, 10, 25 or 50 observations

  if(who == "n0"){
    AllS <- do.call(rbind, lapply(sc.list, function(scenari){
      load(paste(path, scenari, "AllResults_n0.Rdata", sep = "/"))
      AllResults$Scenario <- scenari
      AllResults$SimuN <- rownames(AllResults)
      AllResults
    }))
  }
  if(who == "n5"){
    AllS <- do.call(rbind, lapply(sc.list, function(scenari){
      load(paste(path, scenari, "AllResults_n5.Rdata", sep = "/"))
      AllResults_n5$Scenario <- scenari
      AllResults_n5$SimuN <- rownames(AllResults_n5)
      AllResults_n5
    }))
  }
  if(who == "n10"){
    AllS <- do.call(rbind, lapply(sc.list, function(scenari){
      load(paste(path, scenari, "AllResults_n10.Rdata", sep = "/"))
      AllResults_n10$Scenario <- scenari
      AllResults_n10$SimuN <- rownames(AllResults_n10)
      AllResults_n10
    }))
  }
  if(who == "n25"){
    AllS <- do.call(rbind, lapply(sc.list, function(scenari){
      load(paste(path, scenari, "AllResults_n25.Rdata", sep = "/"))
      AllResults_n25$Scenario <- scenari
      AllResults_n25$SimuN <- rownames(AllResults_n25)
      AllResults_n25
    }))
  }
  if(who == "n50"){
    AllS <- do.call(rbind, lapply(sc.list, function(scenari){
      load(paste(path, scenari, "AllResults_n50.Rdata", sep = "/"))
      AllResults_n50$Scenario <- scenari
      AllResults_n50$SimuN <- rownames(AllResults_n50)
      AllResults_n50
    }))
  }
  AllS$Multicons_Threshold <- rep(who)

  Params <- do.call(rbind, lapply(sc.list, function(diri){
    tmp <- read.table(
      paste(path, diri, "0_Simulation_parameters.txt", sep = "/"),
      header = T, stringsAsFactors = F
    )
    tmp <- tmp[,c("means", "STD.c.per", "md.prop", "covs")]
    tmp$means <- switch(
      tmp$means,
      "list(c(3.2, 4.2, 4.5), c(4.3, 5.4, 2.3), c(5.6, 3, 3.3))" = "Scenario 2",
      "list(c(.7, 3, 5.8), c(3, 5, .8), c(5.6, .5, 3.3))" = "Scenario 1",
      "list(c(1.7, 1.7, 1.7), c(3, 3, 3), c(4.3, 4.3, 4.3))" = "Scenario 3"
    )
    tmp$STD.c.per <- paste(round(eval(parse(text = tmp$STD.c.per)), 1), collapse = "; ")
    tmp$STD.c.per <- switch (as.character(tmp$STD.c.per),
                             "0; 0; 0" = "No cens", "0.2; 0.2; 0.2" = "All cens")
    tmp$md.prop <- switch (as.character(tmp$md.prop),
                           "0.3" = "30p NA", "0" = "No NA")
    tmp$covs <- switch(
      tmp$covs,
      "list(diag(.3, ncol = 3, nrow = 3))" = "No correlation",
      "list(0.3 * matrix(c(1, 0, 0, 0, 1, 0.4, 0, 0.4, 1), ncol = 3, nrow = 3))" = "sigma_x2x3 0.4",
      "list(0.3 * matrix(c(1, 0.95, 0, 0.95, 1, 0, 0, 0, 1), ncol = 3, nrow = 3))" = "sigma_x1x2 0.95"
    )

    tmp$Scenario <- diri
    tmp

  }))

  data <- melt(AllS, id.vars = c("Scenario", "SimuN", "Multicons_Threshold"))
  data$methcl.type <- sapply(data$variable, function(tmp){
    strsplit(as.character(tmp), "..", fixed = T)[[1]][3]
  })
  data$mdmeth.type <- sapply(data$methcl.type, function(tmp){
    strsplit(as.character(tmp), "_", fixed = T)[[1]][1]
  })
  data$clust.type <- sapply(data$methcl.type, function(tmp){
    strsplit(as.character(tmp), "_", fixed = T)[[1]][2]
  })

  data$md.type <- sapply(data$variable, function(tmp){
    strsplit(as.character(tmp), "..", fixed = T)[[1]][1]
  })
  data$ind.type <- sapply(data$variable, function(tmp){
    strsplit(as.character(tmp), "..", fixed = T)[[1]][2]
  })

  data2 <- dcast(data, Scenario + SimuN + Multicons_Threshold +
                   md.type + mdmeth.type + clust.type  ~ ind.type)
  data2 <- left_join(data2, Params, by = "Scenario")

  data2$means <- factor(
    data2$means,
    levels = c("Scenario 1", "Scenario 2", "Scenario 3")
  )[drop = T]
  data2$means <- gsub("Scenario", "Sc.", data2$means, fixed = T)
  data2$STD.c.per <- factor(
    data2$STD.c.per,
    levels = c("No cens", "All cens")
  )[drop = T]
  data2$md.prop <- factor(data2$md.prop, levels = c("No NA", "30p NA"))[drop = T]
  data2$covs <- factor(
    data2$covs,
    levels = c("No correlation", "sigma_x2x3 0.4", "sigma_x1x2 0.95")
  )[drop = T]


  if(what == "left"){
    data2$mdmeth.type <- plyr::revalue(
      factor(data2$mdmeth.type),
      c("CompleteData.ch" = "Complete data \n \n CH",
        "mi.MultiCons.ch" = "Multiple imputation \n Multicons \n CH",
        "mi.MultiCons.CritCF" = "Multiple imputation \n Multicons \n CritCF",
        "mi.Bruckers.ch" = "Multiple imputation \n Combinatorial optimisation \n CH",
        "mi.Bruckers.CritCF" = "Multiple imputation \n Combinatorial optimisation \n CritCF",
        "mi.Basagana.CritCF" = "Multiple imputation \n Relabeling and Voting \n CritCF",
        "std.curve.MultiCons.ch" ="Calibration curve \n \n CH",
        "lod.5.MultiCons.ch" = "LOD/2 \n \n CH",
        "mi.si.ch" = "Stochastic simple imputation \n \n CH"))
  }

  if(what == "missing"){
    data2$mdmeth.type <- plyr::revalue(
      factor(data2$mdmeth.type),
      c("CompleteData.ch" = "Complete data \n \n CH",
        "mi.MultiCons.ch" = "Multiple imputation \n Multicons \n CH",
        "mi.MultiCons.CritCF" = "Multiple imputation \n Multicons \n CritCF",
        "mi.Bruckers.ch" = "Multiple imputation \n Combinatorial optimisation \n CH",
        "mi.Bruckers.CritCF" = "Multiple imputation \n Combinatorial optimisation \n CritCF",
        "mi.Basagana.CritCF" = "Multiple imputation \n Relabeling and Voting \n CritCF",
        "std.curve.MultiCons.ch" ="Calibration curve \n Multicons \n CH",
        "std.curve.Bruckers.ch" ="Calibration curve \n Combinatorial optimisation \n CH",
        "lod.5.MultiCons.ch" = "LOD/2 \n Multicons \n CH",
        "cc.ch" = "CCA \n \n CH",
        "mi.si.ch" = "Stochastic simple imputation \n \n CH",

        "CompleteData" = "Complete data \n \n CH",
        "cc" = "CCA \n \n CH",
        "mi.Bruckers" = "Multiple imputation \n Combinatorial optimisation \n CH",
        "mi.Basagana" = "Multiple imputation \n Relabeling and Voting \n CritCF"
      ))

    data2$md.type <- factor(data2$md.type, levels = c("mcar", 'mar', "mnar", "mnarwk"))
    data2$md.type <- plyr::revalue(data2$md.type, c("mcar" = "MCAR", "mar" = 'MAR',
                                                    "mnar" = "MNAR-1",
                                                    "mnarwk" = "MNAR-2"))

  }

  data2$mdmeth.type <- factor(
    data2$mdmeth.type,
    levels = names(color.values)
  )[drop = T]

  return(data2)
}

compute_Delta <- function(data, ref = "Complete data \n \n CH", var = "mdmeth.type"){
  # calculates the differenec of performances between the
  #    methods and the reference ref
  # data: dataframe containing the performances of all mehtods
  # ref: name of the reference method
  # var : var were to find the reference

  data.diff <- data[data[, var] != ref, ]
  data.tmp <- data[data[, var] == ref, ]

  indexes <- c("ARI", "ARI.cc", "ARI.nocens", "ARI.nona", "Nbclust", "Per.Inclass.cc",
               "Per.Inclass.cens", "Per.Inclass.na", "Per.Inclass.nacens")

  for (i in 1:nrow(data.tmp)){
    whu <- which(data.diff$Scenario == data.tmp$Scenario[i]  &
                   data.diff$SimuN  == data.tmp$SimuN[i] &
                   data.diff$md.type == data.tmp$md.type[i])
    data.diff[whu, indexes] <- data.diff[whu, indexes] -
      matrix(rep(unlist(data.tmp[i, indexes]), length(whu)), nrow = length(whu), byrow = T)
  }

  return(data.diff)
}

getdata.centers <- function(data, reference){
  # calculates mean center and ellipse coordinates
  # data: dataframe of group mean and cov as obtained
  #       by MyCenters.extracteur
  # reference: center and covariance of reference groups

  u <- colnames(reference)
  data.plot <- data.frame(do.call(
    rbind,
    lapply(setdiff(names(data), "CompleteData"), function(mdtype) {
      do.call(rbind, lapply(names(data[[mdtype]]), function(mdmeth) {
        data.frame(
          md.meth = mdmeth,
          md.type = mdtype,
          Sim = c("After imp.", "Exact"),
          rbind(colMeans(data[[mdtype]][[mdmeth]][u], na.rm = T),
                reference)
        )
      }))
    })))

  data.plot.m <- melt(data.plot, id.vars = c("md.meth","md.type", "Sim"))
  data.plot.m$Group <- "G1"
  data.plot.m$Group[grep("G2.", data.plot.m$variable, fixed = T)] <- "G2"
  data.plot.m$Group[grep("G3.", data.plot.m$variable, fixed = T)] <- "G3"

  data.plot.m$Var <- NA
  data.plot.m$Var[grep(".X1.", data.plot.m$variable, fixed = T)] <- "X1"
  data.plot.m$Var[grep(".X2.", data.plot.m$variable, fixed = T)] <- "X2"
  data.plot.m$Var[grep(".X3.", data.plot.m$variable, fixed = T)] <- "X3"
  data.plot.m$Var[grep(".X1X1.", data.plot.m$variable, fixed = T)] <- "X1"
  data.plot.m$Var[grep(".X2X2.", data.plot.m$variable, fixed = T)] <- "X2"
  data.plot.m$Var[grep(".X3X3.", data.plot.m$variable, fixed = T)] <- "X3"

  data.plot.m$smry <- "mean"
  data.plot.m$smry[grep(".sigma", data.plot.m$variable, fixed = T)] <- "sigma"
  data.plot.m[which(data.plot.m$value == "NaN"), "value"] <- NA

  data.plot.m$VAR <- gsub("G..", "", data.plot.m$variable)
  dtemp <- dcast(data.plot.m, value.var = "value",
                 md.meth + md.type + Group + Sim ~ VAR)
  dtemp <- dtemp[complete.cases(dtemp), ]
  data.ellispe3D <- do.call(
    rbind,
    lapply(1:nrow(dtemp), function(i){
      Sig <- matrix(unlist(unname(
        dtemp[i, c("X1X1.sigma", "X1X2.sigma", "X1X3.sigma",
                   "X1X2.sigma", "X2X2.sigma", "X2X3.sigma",
                   "X1X3.sigma", "X2X3.sigma", "X3X3.sigma")])),
        ncol = 3, nrow = 3)

      tt <- ellipse3d(Sig, center = unlist(dtemp[i, c("X1.mean", "X2.mean", "X3.mean")]))
      temp <- data.frame(t(tt$vb)[, 1:3])
      colnames(temp) <- c("X1", "X2", "X3")
      temp[, "X1"] <- temp[, "X1"] + dtemp[i, "X1.mean"]
      temp[, "X2"] <- temp[, "X2"] + dtemp[i, "X2.mean"]
      temp[, "X3"] <- temp[, "X3"] + dtemp[i, "X3.mean"]

      data.frame(md.meth = dtemp[i, "md.meth"],
                 md.type = dtemp[i, "md.type"],
                 Group = dtemp[i, "Group"],
                 Sim = dtemp[i, "Sim"],
                 temp
      )
    }))

  return(list(data.centers = dtemp,
              data.ellispe3D = data.ellispe3D))
}

getEllipse <- function(reference){
  # returns center and ellipse coordinates
  # reference:  center and covariance of reference groups
  u <- colnames(reference)
  data <- reference

  data.m <- melt(data, measure.vars = u)
  data.m$Group <- "G1"
  data.m$Group[grep("G2.", data.m$variable, fixed = T)] <- "G2"
  data.m$Group[grep("G3.", data.m$variable, fixed = T)] <- "G3"
  data.m$VAR <- gsub("G..", "", data.m$variable)
  dtemp <- dcast(data.m, value.var = "value", Group ~ VAR)

  data.ellipse3D <- do.call(
    rbind,
    lapply(1:nrow(dtemp), function(i){
      Sig <- matrix(unlist(unname(
        dtemp[i, c("X1X1.sigma", "X1X2.sigma", "X1X3.sigma",
                   "X1X2.sigma", "X2X2.sigma", "X2X3.sigma",
                   "X1X3.sigma", "X2X3.sigma", "X3X3.sigma")]
      )), ncol = 3, nrow = 3)

      tt <- ellipse3d(Sig, center = unlist(
        dtemp[i, c("X1.mean", "X2.mean", "X3.mean")]
      ))
      temp <- data.frame(t(tt$vb)[, 1:3])
      colnames(temp) <- c("X1", "X2", "X3")
      temp[, "X1"] <- temp[, "X1"] + dtemp[i, "X1.mean"]
      temp[, "X2"] <- temp[, "X2"] + dtemp[i, "X2.mean"]
      temp[, "X3"] <- temp[, "X3"] + dtemp[i, "X3.mean"]

      data.frame(Group = dtemp[i, "Group"], temp)
    })
  )

  return(list(
    data.ellipse3D = data.ellipse3D,
    data.means = dtemp[,c("Group","X1.mean", "X2.mean", "X3.mean")]
  ))
}

int_fm <- function(x, mu1, mu2, sd1, sd2) {
  xdim <- matrix(rep(x, each = length(mu1)),
                 byrow = T, ncol = length(mu1))
  f1 <- mvtnorm::dmvnorm(xdim, mean = mu1, sigma = sd1)
  f2 <- mvtnorm::dmvnorm(xdim, mean = mu2, sigma = sd2)
  pmin(f1, f2)
}


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
