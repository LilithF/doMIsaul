#' plot a PCA from a multiply imputed dataset.
#'
#' \code{plot_MIpca} draws only mean value while \code{plot_MIpca_all} draws all
#'    values for the selected observations.
#'
#' @param data.list list of the imputed datasets.
#' @param obs.sel selection of observations to highlight
#' @param pca.varsel optional. A variables selection for the PCA
#' @param pc.sel which PC to draw. default is 1 and 2
#'
#' @return \code{ggplot} object
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer.imp <- MImpute(cancer[, -c(1:3)], 6)
#' plot_MIpca(cancer.imp, 1:10,
#'     pca.varsel = c("age", "sex", "ph.ecog", "meal.cal",  "wt.loss"))
plot_MIpca <- function(data.list, obs.sel = NULL, pca.varsel = NULL,
                       pc.sel = c(1,2)){

  #Observation ID
  obs.id <- rownames(data.list[[1]])
  if(is.null(obs.id))  obs.id <- 1:nrow(data.list[[1]])
  data.list <- lapply(data.list, function(x) {cbind(ID = obs.id, x)})

  #Data fusion
  db <- as.data.frame(do.call(rbind, data.list))

  #PCA
  if(is.null(pca.varsel)){
    vs <- setdiff(names(!sapply(db, is.factor)), "ID")
  } else {
    vs <- c(pca.varsel)
  }
  myPr <- stats::prcomp(db[, vs], scale = TRUE)
  db.plot <- cbind(db, myPr$x)



  # Mean position
  mean.plot <- dplyr::summarise_all(dplyr::group_by(db.plot, ID), mean)

  #Select observations
  if(is.null(obs.sel)) {obs.sel <- 1:nrow(mean.plot)}
  if(is.numeric(obs.sel)){
    mean.plot$ID.sel <- mean.plot$ID %in% as.character(obs.sel)
  }
  if(is.character(obs.sel)) {
    mean.plot$ID.sel <- eval(parse(text = gsub("DATA", "mean.plot", obs.sel)))
  }
  if(!is.character(obs.sel) & !is.vector(obs.sel)) {
    mean.plot$ID.sel <- rep(FALSE, nrow(mean.plot))
  }
  mean.plot$ID.selname <- ifelse(mean.plot$ID.sel, mean.plot$ID, NA)

  db.plot$ID.sel <- db.plot$ID.selname <- NA
  for (uu in unique(db.plot$ID)){
    db.plot$ID.sel[db.plot$ID %in% uu] <- mean.plot$ID.sel[mean.plot$ID %in% uu]
    db.plot$ID.selname[db.plot$ID %in% uu] <-
      mean.plot$ID.selname[mean.plot$ID %in% uu]
  }

  ellipse.plot <- db.plot[db.plot$ID.sel, ]

  ggplot(mean.plot) +
    aes(eval(parse(text = paste0("PC", pc.sel[1]))),
        eval(parse(text = paste0("PC", pc.sel[2]))),
        colour = ID.selname) +
    geom_point() +
    ggpubr::stat_conf_ellipse(data = ellipse.plot, npoint = 1000) +
    xlab(paste0("PC", pc.sel[1])) +
    ylab(paste0("PC", pc.sel[2])) +
    theme(legend.title = element_blank())

}

#' @rdname plot_MIpca
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer.imp <- MImpute(cancer[, -c(1:3)], 6)
#' plot_MIpca_all(cancer.imp, 1:10,
#'     pca.varsel = c("age", "sex", "ph.ecog", "meal.cal",  "wt.loss"))
plot_MIpca_all <-  function(data.list, obs.sel = NULL, pca.varsel = NULL,
                            pc.sel = c(1,2)){

  #Observation ID
  obs.id <- rownames(data.list[[1]])
  if(is.null(obs.id))  obs.id <- 1:nrow(data.list[[1]])
  data.list <- lapply(data.list, function(x) {cbind(ID = obs.id, x)})

  #Data fusion
  db <- as.data.frame(do.call(rbind, data.list))

  #PCA
  if(is.null(pca.varsel)){
    vs <- setdiff(names(!sapply(db, is.factor)), "ID")
  } else {
    vs <- c(pca.varsel)
  }
  myPr <- stats::prcomp(db[, vs], scale = TRUE)
  db.plot <- cbind(db, myPr$x)



  # Mean position
  mean.plot <- dplyr::summarise_all(dplyr::group_by(db.plot, ID), mean)

  #Select observations
  if(is.null(obs.sel)) {obs.sel <- 1:nrow(mean.plot)}
  if(is.numeric(obs.sel)){
    mean.plot$ID.sel <- mean.plot$ID %in% as.character(obs.sel)
  }
  if(is.character(obs.sel)) {
    mean.plot$ID.sel <- eval(parse(text = gsub("DATA", "mean.plot", obs.sel)))
  }
  if(!is.character(obs.sel) & !is.vector(obs.sel)) {
    mean.plot$ID.sel <- rep(FALSE, nrow(mean.plot))
  }
  mean.plot$ID.selname <- ifelse(mean.plot$ID.sel, mean.plot$ID, NA)

  db.plot$ID.sel <- db.plot$ID.selname <- NA
  for (uu in unique(db.plot$ID)){
    db.plot$ID.sel[db.plot$ID %in% uu] <- mean.plot$ID.sel[mean.plot$ID %in% uu]
    db.plot$ID.selname[db.plot$ID %in% uu] <-
      mean.plot$ID.selname[mean.plot$ID %in% uu]
  }

  ellipse.plot <- db.plot[db.plot$ID.sel, ]

  ggplot(mean.plot[!mean.plot$ID.sel, ]) +
    aes(eval(parse(text = paste0("PC", pc.sel[1]))),
        eval(parse(text = paste0("PC", pc.sel[2]))),
        color = ID.selname) +
    geom_point() +
    geom_point(data = db.plot[db.plot$ID.sel, ], alpha = .4) +
    ggpubr::stat_conf_ellipse(data = ellipse.plot, npoint = 1000) +
    xlab(paste0("PC", pc.sel[1])) +
    ylab(paste0("PC", pc.sel[2])) +
    theme(legend.title = element_blank())

}
