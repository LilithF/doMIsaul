% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CritCF_index_functions.R
\name{CritCF.sel}
\alias{CritCF.sel}
\title{Number of cluster selection according to \code{CritCF} index}
\usage{
CritCF.sel(data, min.nc, max.nc, method, distance)
}
\arguments{
\item{data}{dataframe for which the number of cluster should be estimated.}

\item{min.nc}{integer strictly higher than 1: minimum number of clusters.}

\item{max.nc}{integer (\code{>min.nc}): maximum number of clusters.}

\item{method}{string, clustering algorithm to use. Available values are
"\code{kmeans}", "\code{hc}" (for \code{hclust()}) or "\code{mclust}".}

\item{distance}{string, distance between the observations (either "euclidean"
or "manhattan").}
}
\value{
A list containing the selected number of clusters, the \code{CritCF}
values and the best partition.
}
\description{
Wrapper to estimate the best number of clusters according to \code{CritCF}
index
}
\keyword{internal}
