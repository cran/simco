\name{simco-package}
\alias{simco-package}
\alias{simco}
\docType{package}
\title{
Simco: a package to carry out similarity coefficient calculations from Structure output files
}
\description{
Simco is a package that (1) imports Structure output files into R and (2) carries out similarity coefficient calculations on them
}
\details{
\tabular{ll}{
Package: \tab simco\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-04-4\cr
License: \tab GNU General Public Licence\cr
}
There are two functions included in this package:

\code{SimCoImport}: the import function, designed to import and parse data directly from the Structure output files.

\code{SimCoef}: the function to do the similarity coefficient analysis.

Examples of Structure output files, and further help, are provided in the \code{docs} directory of the \code{simco} package (which should be in your R library folder if you have installed \code{simco} correctly).

}
\author{
Owen Jones

Maintainer: Owen Jones 
\email{owen.jones@imperial.ac.uk}
}
\references{
Rebbeck, CA, Jones, OR, & Tsai, IJ  (Submitted to \emph{Bioinformatics}) SimCo: A program to automate the comparison of multiple structure runs (available from \email{owen.jones@imperial.ac.uk})

Pritchard, J. K. et al. (2000) Inference of population structure using multilocus genotype data. \emph{Genetics}. 155, 945-959.
}


\keyword{package}

\seealso{ \code{\link{SimCoImport}}  
\code{\link{SimCoef}} 
}
\examples{

}
