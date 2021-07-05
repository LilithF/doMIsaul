# data.unsup.m1 <- data.frame(data.unsup.miced[[1]],
#                             hemato[, c(var.cat.main, var.cat.comorb, var.cat.hemato, var.cat.sympt, var.cat.covid)],
#                             Unsup.part = Unsup.part.upd.NA, stringsAsFactors = T)
# tab.cont <- do.call(rbind, lapply(var.cont, anova.tab, data = data.unsup.m1, main.var = "Unsup.part"))
# tab.cont$pvalue <- sapply(tab.cont$pvalue, format.pv)
# tab.cont$Parameter <- mapvalues(tab.cont$Parameter, db.names, eng.names, warn_missing = F)
# tab.cont[is.na(tab.cont)] <- ""
# cat("Continuous parameters (used for clustering)")
# kable(tab.cont)
