#' MultiCons Consensus Clustering Algorithm
#'
#' Performs MultiCons clustering, from Al-Najdi et Al.
#' For some reason, if you want to use \code{mclust()} clustering, the package
#' needs to be loaded manually
#'
#' @param DB Either data or dataframe of partitions.
#' @param Clust_entry Is DB partitions (\code{TRUE}) or data (\code{FALSE}).
#' @param Clustering_selection If DB is data, clustering algorithm to select
#'   among. Must be included in default value.
#' @param num_algo Number of clustering algorithms to perform.
#' @param maxClust Maximum number of clusters.
#' @param sim.indice Index for defining best partition. Passed to
#'   \code{clusterCrit::extCriteria)()}, see
#'   \code{clusterCrit::getCriteriaNames(FALSE)} for other available indexes.
#'   If more than one index are given, only the first one will be used.
#' @param returnAll Should all partitions (\code{TRUE}) or only the best
#'  (\code{FALSE}) be returned.
#' @param Plot Should tree be plotted.
#' @param verbose Passed on to \code{mclust()} and other functions.
#'
#' @return A list of 2: performances and partitions. If \code{returnAll} is
#'   \code{TRUE}, both elements of the list contain results for all levels of
#'   the tree, else they only contain the results for the best level of
#'   the tree.
#' @export
#'
#' @examples
#' library(mclust)
#' ### With clustering algorithm choices
#' MultiCons(iris[, 1:4],
#'           Clustering_selection = c("kmeans", "pam", "DIANA", "MCLUST"),
#'           Plot = TRUE)
#' ### With a manual clustering entry
#' parts <- data.frame(factor(rep(c(1,2,3), each = 50)),
#'                     factor(rep(c(1,2,3), times = c(100, 25, 25))),
#'                     factor(rep(c(1,2), times = c(50, 100))),
#'                     factor(rep(c(3, 2, 1), times = c(120, 10, 20))),
#'                     stringsAsFactors = TRUE)
#' MultiCons(parts, Clust_entry = TRUE, Plot = TRUE)
MultiCons <-
  function(DB, Clust_entry = FALSE,
           Clustering_selection = c("kmeans", "pam", "OPTICS", "agghc",
                                    "AGNES", "DIANA", "MCLUST", "CMeans",
                                    "FANNY", "BaggedClust"),
           num_algo = 10, maxClust = 10, sim.indice = "Jaccard",
           returnAll = FALSE, Plot = TRUE, verbose = FALSE){

    if(Plot) {
      requireNamespace("igraph", quietly = FALSE)
      requireNamespace("RColorBrewer", quietly = FALSE)
      requireNamespace("graphics", quietly = FALSE)
    }

    #  1 Generate multiple base clusterings of the dataset;
    if (!Clust_entry){
      if("OPTICS" %in% Clustering_selection) {
        requireNamespace("dbscan", quietly = FALSE)
      }
      if("MCLUST" %in% Clustering_selection) {
        requireNamespace("mclust", quietly = FALSE)
      }
      if (length(intersect(c("pam", "AGNES", "DIANA", "FANNY"),
                           Clustering_selection)) > 0) {
        requireNamespace("cluster", quietly = FALSE)
      }
      if (length(intersect(c("CMeans", "BaggedClust"),
                           Clustering_selection)) > 0) {
        requireNamespace("e1071", quietly = FALSE)
      }

      lesBaseClust <- sample(Clustering_selection, num_algo, replace = TRUE)

      Values <- scales::rescale(1:num_algo, to = c(2, maxClust))

      Clust <- lapply(1:num_algo, function(i) {

        nclust <- max(round(stats::rnorm(1, Values[i], 2)), 2)
        switch(lesBaseClust[i],
               kmeans = stats::kmeans(DB, nclust)$cluster,
               pam = cluster::pam(DB, nclust)$cluster,
               OPTICS = dbscan::extractDBSCAN(dbscan::optics(DB,
                                                             sample(1:10, 1)),
                                              eps_cl = stats::runif(1, 0.3, 1)
                                              )$cluster,
               agghc = stats::cutree(stats::hclust(
                 stats::dist(DB),
                 method = c("ward.D", "ward.D2", "single",
                            "complete", "average", "mcquitty",
                            "median",
                            "centroid")[sample(1:8, 1)]),
                 nclust),
               AGNES = stats::cutree(cluster::agnes(stats::dist(DB)), nclust),
               DIANA = stats::cutree(cluster::diana(stats::dist(DB)), nclust),
               MCLUST = mclust::Mclust(DB, G = 2:maxClust, verbose = FALSE,
                                       modelNames = sample(
                                         mclust::mclust.options("emModelNames"),
                                         1))$classification,
               CMeans = e1071::cmeans(DB, nclust)$cluster,
               FANNY = cluster::fanny(DB, nclust)$cluster,
               BaggedClust = e1071::bclust(DB,
                                           centers = min(20, nclust),
                                           verbose = FALSE)$cluster)
      })

      Selo <- which(lesBaseClust == "OPTICS")

      miaou <- if (!is.null(Selo)) {
        lapply(Selo, function(i) {
          while (all(Clust[[i]] %in% c(0, 1))) {
            Clust[[i]] <<-
              dbscan::extractDBSCAN(dbscan::optics(DB, sample(1:10, 1)),
                                    eps_cl = stats::runif(1, 0.1, 1))$cluster
          }
        })
      }


    }else{

      Clust <-
        DB[, sapply(1:ncol(DB), function(i) {
          length(levels(DB[, i])) > 1
        }, simplify = TRUE, USE.NAMES = TRUE)]
      num_algo <- ncol(Clust)
    }

    #  2 Build the cluster membership matrix M;
    withr::with_options(
      list(na.action='na.pass'),
      Clusters <- do.call(cbind, lapply(Clust, function(i) {
        base <- stats::model.matrix( ~ as.factor(i) + 0)
        base[, !grepl("\\)0", colnames(base))]
      }))
    )

    withr::local_options(list(na.action='na.omit'))
    n <- nrow(Clusters)
    p <- ncol(Clusters)
    Clusters <- apply(Clusters, 2, as.logical)

    #  3 Generate FCPs from M for minsupport = 0;
    # To generate the frequent closed patterns, we use arules
    Clusters <- as.data.frame(Clusters)
    colnames(Clusters) <- 1:ncol(Clusters)
    FCP <- arules::apriori(Clusters,
                           parameter = list(target = "closed frequent itemsets",
                                            # support = 10 / (n),
                                            support = 1 / (n * p),
                                            maxlen = max(num_algo, 10),
                                            # maxlen = num_algo,
                                            maxtime = max(num_algo, 60)),
                           control = list (verbose = FALSE))

    FCI <- methods::as(FCP@items, "matrix")
    Indiv <- apply(FCI, 1, function(x) {
      base <- as.matrix(Clusters[, x])
      which(apply(base, 1, sum) == ncol(base))
    })

    #  4 Sort the FCPs in ascending order of the size of their instance list;
    FCP <-
      lapply(order(sapply(Indiv, length, simplify = TRUE, USE.NAMES = TRUE)),
             function(i) {
               list(FCI = which(FCI[i, ] == TRUE), Indiv = Indiv[[i]])
             })

    #  5 BiClust <- {instance sets of FCPs built from numalgo base clusters};
    Sel <-
      unlist(sapply(1:length(FCP), function(i) {
        if (length(FCP[[i]][[1]]) == num_algo)
          return(i)
      }, simplify = TRUE, USE.NAMES = TRUE))
    BiClust <- lapply(Sel, function(i) {
      FCP[[i]][[2]]
    })

    #  6 Assign a label to each set in BiClust to build the first
    # consensus vector and store it in a list of vectors ConsVctrs;
    ConsVctrs <- list(BiClust)
    bic <- rep(NA, n)
    sapply(1:length(BiClust), function(i) {
      bic[BiClust[[i]]] <<- i
    }, simplify = TRUE, USE.NAMES = TRUE)

    distjack <-
      sum(sapply(Clust, function(x){
        clusterCrit::extCriteria(as.integer(x),
                                 as.integer(bic),
                                 crit = sim.indice)[[1]]

      }, simplify = TRUE, USE.NAMES = TRUE)) / num_algo

    TSim <- distjack

    DTs <- num_algo
    ST <- rep(1, num_algo)
    #  /* Build the remaining consensuses */;
    #  7 for DT = (numalgo - 1) to 1 do
    for (DT in (num_algo - 1):1) {
      # 8 BiClust <- BiClust U {instance sets of FCPs
      # built from DT base clusters};
      Sel <-
        unlist(sapply(1:length(FCP), function(i) {
          if (length(FCP[[i]][[1]]) == DT)
            return(i)
        }, simplify = TRUE, USE.NAMES = TRUE))
      BiClust <- c(BiClust, lapply(Sel, function(i) {
        FCP[[i]][[2]]
      }))
      # 9 N <-|BiClust| // Nbr of sets in BiClust;
      N <- length(BiClust)
      # 10 for i = 1 to N do
      for (i in 1:N) {
        # 11 Bi<-ith set in BiClust;
        Bi <- BiClust[[i]]
        # 12 for j = 1 to N, j = i do
        j <- i + 1
        while (j != (N + 1)) {
          Bj <- BiClust[[j]]
          if (length(intersect(Bi, Bj)) != 0) {
            BiClust[[j]] <- sort(union(Bi, Bj))
            BiClust[[i]] <- "Lapin"
            j <- N + 1
          } else{
            j <- j + 1
          }
        }
      }
      lapply(rev(which(BiClust == "Lapin")), function(i) {
        BiClust[[i]] <<- NULL
      })
      lala <- sapply(BiClust, function(i) {
        paste0(i, collapse = "")
      }, simplify = TRUE, USE.NAMES = TRUE)
      lala2 <-
        sapply(ConsVctrs[[length(ConsVctrs)]], function(i) {
          paste0(i, collapse = "")
        }, simplify = TRUE, USE.NAMES = TRUE)
      if (length(lala2) == length(lala) & all(lala2 %in% lala)) {
        ST[length(ConsVctrs)] <- ST[length(ConsVctrs)] + 1
        ST <- ST[-length(ST)]
      } else{
        DTs <- c(DTs, DT)
        ConsVctrs <- c(ConsVctrs, list(BiClust))
        bic <- rep(NA, n)
        sapply(1:length(BiClust), function(i) {
          bic[BiClust[[i]]] <<- i
        }, simplify = TRUE, USE.NAMES = TRUE)
        distjack <-
          sum(sapply(Clust, function(x) {

            clusterCrit::extCriteria(as.integer(x),
                                     as.integer(bic),
                                     crit = sim.indice)[[1]]

          }, simplify = TRUE, USE.NAMES = TRUE
          )) / num_algo
        TSim <- c(TSim, distjack)
      }
    }

    #       /* Find the consensus the most similar to the ensemble */
    RecomCons <- DTs[which.max(TSim)]
    L <- length(ConsVctrs)
    names(ConsVctrs) <- LETTERS[1:L]
    taille <- lapply(1:L, function(i) {
      sapply(ConsVctrs[[i]], length, simplify = TRUE, USE.NAMES = TRUE)
    })
    lapply(1:L, function(i) {
      names(ConsVctrs[[i]]) <<-
        paste0(LETTERS[i], 1:length(ConsVctrs[[i]]), "(", taille[[i]], ")")
    })

    if (Plot) {
      Mat <- do.call(rbind, lapply(1:(L - 1), function(i){
        vers <- sapply(ConsVctrs[[i]], function(clust){
          names(ConsVctrs[[i + 1]])[which(
            sapply(ConsVctrs[[i + 1]],
                   function(a){
                     any(clust %in% a)
                   }, simplify = TRUE, USE.NAMES = TRUE))]
        }, simplify = TRUE, USE.NAMES = TRUE)
        cbind(names(ConsVctrs[[i]]), vers)
      }))
      df <- data.frame(Mat)
      df_graph <- igraph::graph_from_data_frame(df, directed = FALSE)
      palettes <-
        RColorBrewer::brewer.pal(n = min(9, L),
                                 name = "Set3")[1:L - 9 * (1:L %/% 9)]
      ts <- unlist(taille)
      ts[(nrow(Mat) + 1):length(ts)] <-
        ts[(nrow(Mat) + 1):length(ts)][unique(
          as.numeric(sapply(
            Mat[grepl(LETTERS[L], Mat[, 2]), 2], function(x) {
              substr(x, 2, 2)
            }, simplify = TRUE, USE.NAMES = TRUE))
        )]
      names(ts) <- NULL
      igraph::V(df_graph)$color <-
        unlist(sapply(1:L, function(i) {
          rep(palettes[i], length(taille[[i]]))
        }, simplify = TRUE, USE.NAMES = TRUE))
      igraph::V(df_graph)$size <- scales::rescale(ts, to = c(10, 20))
      igraph::V(df_graph)$frame.color <- rep("white", length(ts))
      igraph::V(df_graph)$label <- ts
      Better <- names(ConsVctrs[[which.max(TSim)]])

      withr::local_par(list(mar = c(0, 0, 0, 0)))
      p <- igraph::plot.igraph(
        df_graph,
        layout = igraph::layout_as_tree(
          df_graph,
          root = names(ConsVctrs[[length(ConsVctrs)]])),
        mark.groups = list(Better),
        mark.col = "lightblue",
        mark.border = "black",
        margin = c(-0.1, -1.5, 0, -0.5)
      )
      graphics::legend (
        "right",
        title = paste("Advised DT=", RecomCons,
                      "\n", "Similarity=", round(max(TSim), 2)),
        fill = rev(palettes),
        legend = paste("DT=", rev(DTs), "ST=",
                       rev(ST), "Sim=", rev(round(TSim, 2))),
        bty = "n"
      )
    }
    if (returnAll) {
      Recap <- rbind(DTs, ST, TSim)
      colnames(Recap) <- LETTERS[1:L]
      return(list(Performances = Recap, Partitions = ConsVctrs))
    } else{
      i <- which.max(TSim)
      bic <- rep(NA, n)
      sapply(1:length(ConsVctrs[[i]]), function(j) {
        bic[ConsVctrs[[i]][[j]]] <<- j
      }, simplify = TRUE, USE.NAMES = TRUE)
      list(
        Performances = c(
          DT = DTs[i],
          ST = ST[i],
          Similarity = TSim[i]
        ),
        Partitions = bic
      )
    }
  }
