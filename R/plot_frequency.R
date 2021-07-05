# temp <-  data.frame(do.call(cbind,
#                             lapply(c(var.cat.main, var.cat.comorb[1]), function(x){
#                               hemato2[, x] <- mapvalues(hemato2[, x], NA, "NA")
#                               model.matrix(as.formula(paste("~ 0 +", x)) , data = hemato2)
#                             })),
#                     hemato2[,c(var.cat.comorb[-1], var.cat.hemato[c(1,3)])],
#                     lapply(c(var.cat.hemato[2]), function(x){
#                       hemato2[, x] <- mapvalues(hemato2[, x], NA, "NA")
#                       model.matrix(as.formula(paste("~ 0 +", x)) , data = hemato2)
#                     }),
#                     hemato2[,c(var.cat.sympt[-1])],
#                     lapply(c(var.cat.sympt[1], var.cat.covid[2]), function(x){
#                       hemato2[, x] <- mapvalues(hemato2[, x], NA, "NA")
#                       model.matrix(as.formula(paste("~ 0 +", x)) , data = hemato2)
#                     }),
#                     hemato2[,c( var.cat.covid[-2])],
#                     Unsup = hemato2$Unsup)
# temp.melt <- melt(temp, id.vars = "Unsup")
# temp.melt$variable <- gsub("Country", "Country: ", temp.melt$variable)
# temp.melt$variable <- gsub("Sex.b1", "Sex: Male", temp.melt$variable)
# temp.melt$variable <- gsub("Diagh.f5", "Diagnosis: ", temp.melt$variable)
# temp.melt$variable <- gsub("Diagh.f2", "Diagnosis(K): ", temp.melt$variable)
# temp.melt$variable <- gsub("iCU", "ICU: ", temp.melt$variable)
# temp.melt$variable <- gsub("chemo_type", "CT type: ", temp.melt$variable)
# temp.melt <- temp.melt[!temp.melt$variable %in% c("Diagnosis(K): Benigne", "Country: France", "ncomorb.f2.0", "Sex.b0"), ]
# temp.melt$variable <- gsub("ncomorb.f20", "No comorbidities", temp.melt$variable)
# temp.melt$variable <- gsub("delsf.f", "Delay 1st sympt.: ", temp.melt$variable)
# temp.melt$variable <- gsub(".0.7.", "]0,7]", temp.melt$variable)
# temp.melt$variable <- gsub("..0", "<=0", temp.melt$variable, fixed = T)
# temp.melt$variable <- gsub(".7", ">7", temp.melt$variable, fixed = T)
# temp.melt <- temp.melt[!temp.melt$variable %in% c("Delay 1st sympt.: Asymptomatic."), ]
# temp.melt$variable <- factor(temp.melt$variable, levels = rev(unique(temp.melt$variable)))
#
# temp.melt2 <- temp.melt %>%
#   group_by(variable, Unsup) %>%
#   summarise(freq = 100*sum(value, na.rm = T)/n())
# temp.melt2$variable <- mapvalues(temp.melt2$variable, db.names, eng.names)
#
# nlev <- nlevels(Unsup.part.upd.NA)
#
# pps<- lapply(levels(Unsup.part.upd.NA), function(xx){
#   my.col <- mapvalues(xx,
#                       levels(Unsup.part.upd.NA),
#                       RColorBrewer::brewer.pal(nlev, "Set1")[1:nlev],
#                       warn_missing = F)
#   p <-  ggplot(temp.melt2[temp.melt2$Unsup %in% xx, ]) +
#     geom_bar(aes(x = variable, y = freq, fill = freq), stat = "identity") +
#     scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 100), name = "Prevalence in the cluster (%)") +
#     coord_flip(ylim = c(0,100)) +
#     geom_vline(xintercept = 2.5, size = .5, linetype = 2)+
#     geom_vline(xintercept = 5.5, size = 1)+
#     geom_vline(xintercept = 8.5, size = .5, linetype = 2)+
#     geom_vline(xintercept = 19.5, size = 1) +
#     geom_vline(xintercept = 23.5, size = .5, linetype = 2)+
#     geom_vline(xintercept = 25.5, size = 1) +
#     geom_vline(xintercept = 34.5, size = 1) +
#     geom_vline(xintercept = 39.5, size = .5, linetype = 2) +
#     geom_vline(xintercept = 40.5, size = 1) +
#
#     theme(axis.text.x = element_blank(),
#           axis.text.y = element_text(color = "black", size = 12),
#           axis.ticks.x = element_blank(),
#           axis.title.x = element_text(color = my.col, face = "bold"),
#           legend.position = "bottom",
#           axis.title.y = element_blank()) +
#     ylab(paste0(xx, "\n(n=", sum(hemato2$Unsup == xx) ,")")) +
#     xlab("Clinical parameters")
#   leg <<- ggpubr::get_legend(p)
#   if(xx %in% levels(Unsup.part.upd.NA)[1]){
#     p + theme(legend.position = "none")
#   } else {
#     p + theme(legend.position = "none",
#               axis.text.y = element_blank(),
#               axis.ticks.y = element_blank(),
#               axis.title.y = element_blank())
#   }
# })
# p.UnsupComorb <- cowplot::plot_grid(plotlist = pps, nrow = 1, rel_widths = c(1, rep(.4, length(pps)-1)))
# p.UnsupComorb <- cowplot::plot_grid(p.UnsupComorb, ggpubr::as_ggplot(leg), ncol = 1, rel_heights = c(1,.05))
# print(p.UnsupComorb)
