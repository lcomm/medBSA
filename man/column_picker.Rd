% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{column_picker}
\alias{column_picker}
\title{Select elements from a matrix based on a vector of column indices
(Rcpp version)}
\usage{
column_picker(M, v)
}
\arguments{
\item{M}{The n by m matrix}

\item{v}{The length-n vector taking integer values 1 to m}
}
\value{
Vector of what was in each column
}
\description{
From a matrix and a vector, return a vector where the ith element is the
matrix element (i, v[i])
}
\details{
Faster version of M[cbind(1:length(v), v)]
}

