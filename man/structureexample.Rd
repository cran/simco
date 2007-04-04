\name{structureexample}
\alias{structureexample}
\docType{data}
\title{An example dataset for use with SimCo}
\description{This dataset includes 3 runs  of Structure (labelled A, B and C) with 3 clusters assumed. 
Each row represents a tissue sample from a dog. Columns 1 and 2 are simply labels, columns 3-5 
are the probabilities that Structure has assigned to membership of the cluster. Column 6 indicates 
which run the data come from (A, B or C).}
\usage{data(structureexample)}
\format{
\describe{A data frame with 222 observations on the following 6 variables.}
}
\details{A data frame with 222 observations on the following 6 variables. This dataset includes 3 runs  of Structure (labelled A, B and C) with 3 clusters assumed. 
Each row represents a tissue sample from a dog. Columns 1 and 2 are simply labels, columns 3-5 
are the probabilities that Structure has assigned to membership of the cluster. Column 6 indicates 
which run the data come from (A, B or C).}
\source{ Elaine Ostrander (unpublished data)}
\references{
Pritchard, J. K. et al. (2000) Inference of population structure using multilocus genotype data. \emph{Genetics}. 155, 945-959.
}
\examples{
data(structureexample)
}
\keyword{datasets}
