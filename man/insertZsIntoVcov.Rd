% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insertZsIntoVcov.R
\name{insertZsIntoVcov}
\alias{insertZsIntoVcov}
\title{Insert rows/columns with offdiagonal zeros into an existing matrix}
\usage{
insertZsIntoVcov(mat, diagElem, p = length(diagElem))
}
\arguments{
\item{mat}{Existing variance-covariance matrix.  Columns must be named.}

\item{diagElem}{Scalar or vector containing the diagonal entries for the
inserted rows/columns.  If the length of diag is 1 and less than p, then
diagElem will be the diagonal element for every inserted row/column.}

\item{p}{Number of rows and columns to insert. If not specified, then the 
default is to use the length of diagElem}
}
\value{
A square matrix of dimension p greater than the matrix supplied
}
\description{
This functions takes a square matrix and adds columns and rows corresponding
to additional Z variables.  Off-diagonal elements in the added rows and 
columns will be 0, and diagonal elements will be whatever is passed in via 
the "diagElem" parameter.  The columns/rows will be inserted after the last
"Z#" and will be named "Z(# + 1)" through "Z(# + p)", where p is the number
of columns and rows to insert.
}
\examples{
##Make an matrix with the right column names
myMat <- matrix(1:25, nrow = 5, ncol = 5)
colnames(myMat) <- c("A", "Z1", "Z2", "U", "M")

##Insert 3 new columns/rows, with diagonal entries of 100, 200, and 300
insertZsIntoVcov(myMat, diagElem = c(100, 200, 300))
insertZsIntoVcov(myMat, diagElem = 100, p = 4)

}

