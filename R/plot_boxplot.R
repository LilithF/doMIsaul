# include.unclass = FALSE


# temp <- melt(hemato2, measure.vars = setdiff(var.cont, var.AR))
# temp$variable <- mapvalues(temp$variable, db.names, eng.names)
# p.unsup.cnt <- ggplot(temp[!temp$Unsup %in% "Unclassified", ], aes(x = Unsup, y = value)) +
#   geom_boxplot(aes(fill = Unsup)) +
#   # facet_grid(variable~., scales = "free_y") +
#   facet_wrap(~variable, scales = "free_y", ncol = 2) +
#   theme(legend.position = "none",
#         axis.title = element_blank())+
#   scale_fill_brewer(palette = "Set1")
# print(p.unsup.cnt)
