% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pareto.R
\name{pareto}
\alias{pareto}
\title{Pareto optimization}
\usage{
pareto(bi.objective, obj.names = c("Cluster", "Regression"))
}
\arguments{
\item{bi.objective}{dataframe or matrix containing the objective values for
each case}

\item{obj.names}{name of the two objectives (should be in
\code{colnames(bi.objetive)}).}
}
\value{
the entry bi.objective with an additional column "Dominated" with 1
for non optimal solutions and 0 for optimal.
}
\description{
Find Pareto optimal and Pareto non optimal solutions
}
\keyword{internal}
