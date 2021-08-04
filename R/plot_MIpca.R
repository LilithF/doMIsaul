#' Plot a PCA from a multiply imputed dataset.
#'
#' \code{plot_MIpca()} plots only mean value while \code{plot_MIpca_all()}
#'   plots all values for the selected observations.
#'
#' @param data.list The list of the imputed datasets.
#' @param obs.sel The selection of observations to highlight. If \code{NULL},
#'   no observations are selected; if
#'   \code{numeric}, the vector corresponds to the observations' row number to
#'   highlight, if \code{character}, the string should be of type a condition
#'   (\code{TRUE/FALSE}) on the dataset to select the observations, where the
#'   dataset is referred to as \code{"DATA"} (ex: \code{obs.sel = "DATA$X1>3"}).
#' @param color.var Either \code{NULL} to color according to \code{obs.sel},
#'   \code{"none"} to use no color, or a vector of size
#'   \code{nrow(data.list[[1]])} (a factor).
#' @param pca.varsel optional. A vector of strings containing the names of the
#'   variables to use for the PCA. If \code{NULL} all variables in the dataset
#'   will be used.
#' @param pc.sel Numeric vector of size 2 containing the indexes of the
#'   principal components to plot. Default is PC1 and PC2.
#' @param alpha Transparency level for plotting the point of the selected
#'    observations.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer.imp <- MImpute(cancer[, -c(1:3)], 4)
#' plot_MIpca(cancer.imp, 1:10,
#'     pca.varsel = c("age", "sex", "ph.ecog", "meal.cal",  "wt.loss"))
#'
#' plot_MIpca(cancer.imp, obs.sel = NULL, color.var = factor(cancer$status),
#'            pca.varsel = c("age", "sex", "ph.ecog", "meal.cal",  "wt.loss"))
plot_MIpca <- function(data.list, obs.sel, color.var = NULL, pca.varsel = NULL,
                       pc.sel = c(1,2)){
  if(length(pc.sel) != 2)
    stop("pc.sel must be of size 2.")
  if(!is.numeric(pc.sel))
    stop("pc.sel must be numeric and contain the PC indexes to plot.")
  if(any(pc.sel > ncol(data.list[[1]])))
    stop("pc.sel must be numeric and contain the PC indexes to plot.")

  #Observation ID
  obs.id <- rownames(data.list[[1]])
  if(is.null(obs.id))  obs.id <- 1:nrow(data.list[[1]])

  if(length(color.var) == nrow(data.list[[1]])){
    data.list <- lapply(data.list, function(x) {
      data.frame(ID = obs.id, x, color.var)
    })
    COL <- 1
  } else {
    data.list <- lapply(data.list, function(x) {cbind(ID = obs.id, x)})
    COL <- 0
  }


  #Data fusion
  db <- as.data.frame(do.call(rbind, data.list))

  #PCA
  if(is.null(pca.varsel)){
    vs <- setdiff(names(!sapply(db, is.factor)), c("ID", "color.var"))
  } else {
    vs <- c(pca.varsel)
  }
  myPr <- stats::prcomp(db[, vs], scale = TRUE)
  PC.per <- summary(myPr)$importance[2, pc.sel]

  db.plot <- data.frame(cbind(
    db[, intersect(c(vs, "ID", "color.var"), colnames(db))],
    myPr$x))

  # Mean position
  mean.plot <- dplyr::summarise_all(
    dplyr::group_by(db.plot[setdiff(colnames(db.plot), "color.var")], .data$ID),
    mean
    )
  if(COL){
    mean.plot$color.var <-
      factor(unname(unlist(dplyr::summarise(dplyr::group_by(db.plot, .data$ID),
                                            unique(color.var))[, 2])))
  }
  #Select observations
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
  db.plot$ID.selname <- NA
  db.plot$ID.sel <- NA
  for (uu in unique(db.plot$ID)){
    db.plot$ID.sel[db.plot$ID %in% uu] <- mean.plot$ID.sel[mean.plot$ID %in% uu]
    db.plot$ID.selname[db.plot$ID %in% uu] <-
      mean.plot$ID.selname[mean.plot$ID %in% uu]
  }

  ellipse.plot <- db.plot[db.plot$ID.sel, ]
  if(!COL) {
    if(is.null(color.var)){
      p <- ggplot(mean.plot) +
        aes(eval(parse(text = paste0("PC", pc.sel[1]))),
            eval(parse(text = paste0("PC", pc.sel[2]))),
            color = .data$ID.selname)
    } else {
      if(color.var == "none"){
        p <- ggplot(mean.plot) +
          aes(eval(parse(text = paste0("PC", pc.sel[1]))),
              eval(parse(text = paste0("PC", pc.sel[2]))),
              group = .data$ID.selname)
      }
    }
  } else {
      p <- ggplot(mean.plot) +
      aes(eval(parse(text = paste0("PC", pc.sel[1]))),
          eval(parse(text = paste0("PC", pc.sel[2]))),
          group = .data$ID.selname,
          color = color.var)
  }

  p + geom_point() +
    ggpubr::stat_conf_ellipse(data = ellipse.plot, npoint = 100) +
    xlab(paste0("PC", pc.sel[1], " (", round(100*PC.per[1], 2), "%)")) +
    ylab(paste0("PC", pc.sel[2], " (", round(100*PC.per[2], 2), "%)")) +
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
plot_MIpca_all <-  function(data.list, obs.sel, pca.varsel = NULL,
                            color.var = NULL, pc.sel = c(1,2), alpha = .4){

  if(length(pc.sel) != 2)
    stop("pc.sel must be of size 2.")
  if(!is.numeric(pc.sel))
    stop("pc.sel must be numeric and contain the PC indexes to plot.")
  if(any(pc.sel > ncol(data.list[[1]])))
    stop("pc.sel must be numeric and contain the PC indexes to plot.")

  #Observation ID
  obs.id <- rownames(data.list[[1]])
  if(is.null(obs.id))  obs.id <- 1:nrow(data.list[[1]])

  if(length(color.var) == nrow(data.list[[1]])){
    data.list <- lapply(data.list, function(x) {
      data.frame(ID = obs.id, x, color.var)
    })
    COL <- 1
  } else {
    data.list <- lapply(data.list, function(x) {cbind(ID = obs.id, x)})
    COL <- 0
  }


  #Data fusion
  db <- as.data.frame(do.call(rbind, data.list))

  #PCA
  if(is.null(pca.varsel)){
    vs <- setdiff(names(!sapply(db, is.factor)), c("ID", "color.var"))
  } else {
    vs <- c(pca.varsel)
  }
  myPr <- stats::prcomp(db[, vs], scale = TRUE)
  db.plot <- data.frame(cbind(db, myPr$x))

  # Mean position
  mean.plot <- dplyr::summarise_all(
    dplyr::group_by(db.plot[setdiff(colnames(db.plot), "color.var")], .data$ID),
    mean
  )
  if(COL){
    mean.plot$color.var <-
      factor(unname(unlist(dplyr::summarise(dplyr::group_by(db.plot, .data$ID),
                                            unique(color.var))[, 2])))
  }
  #Select observations
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
  if(!COL) {
    if(is.null(color.var)){
      p <- ggplot(mean.plot[!mean.plot$ID.sel, ]) +
        aes(eval(parse(text = paste0("PC", pc.sel[1]))),
            eval(parse(text = paste0("PC", pc.sel[2]))),
            color = .data$ID.selname)
    } else {
      if(color.var == "none"){
        p <- ggplot(mean.plot[!mean.plot$ID.sel, ]) +
          aes(eval(parse(text = paste0("PC", pc.sel[1]))),
              eval(parse(text = paste0("PC", pc.sel[2]))),
              group = .data$ID.selname)
      }
    }
  } else {
    p <- ggplot(mean.plot[!mean.plot$ID.sel, ]) +
      aes(eval(parse(text = paste0("PC", pc.sel[1]))),
          eval(parse(text = paste0("PC", pc.sel[2]))),
          group = .data$ID.selname,
          color = color.var)
  }

  p + geom_point() +
    geom_point(data = db.plot[db.plot$ID.sel, ], alpha = alpha) +
    ggpubr::stat_conf_ellipse(data = ellipse.plot, npoint = 100) +
    xlab(paste0("PC", pc.sel[1])) +
    ylab(paste0("PC", pc.sel[2])) +
    theme(legend.title = element_blank())

}
