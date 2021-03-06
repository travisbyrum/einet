% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/causal-emergence.R
\name{mb}
\alias{mb}
\title{Create Markov Blanket}
\usage{
mb(graph, nodes = igraph::V(graph))
}
\arguments{
\item{graph}{igraph or matrix object.}

\item{nodes}{Numeric vector of vertices.}
}
\value{
A \code{list} of node descendants, parents, and neighbors.
}
\description{
Given a graph and a specified \code{vector} of internal node(s), returns
the parents, the children, and the parents of the children of the
internal node(s).
}
