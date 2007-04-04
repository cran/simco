\name{SimCoef}
\alias{SimCoef}
\title{ A function to carry out the calculation of similarity coefficients from multiple Structure output files. }
\description{
This function takes multiple Structure files that have already been imported using the \code{"SimCoImport()"} function and calculates similarity coefficients for them.\cr

The output is printed to the screen and includes the number of populations (K) that are assumed, the number of individuals (I), the number of Structure runs that have been analysed, the range, median, mean similariy coefficient, and standard error of the mean. Lastly, the output displays the similarity coefficients and the pairs of runs that were compared.\cr
}
\usage{
SimCoef(mydata)
}
\arguments{
  \item{mydata}{ mydata is a dataframe generated using \code{"SimCoImport()"} which is the Structure output files, appended and labelled with a capital letter. }
}
\details{
An accompanying PDF is given with this package with further details of use.}
\value{ 
\item{Number of populations (K)}{The number of populations that were assumed when generating the Strucure files}
\item{Number of individuals (I)}{The number of individuals/samples in the datafile}
\item{Number of Structure runs}{The number of Structure runs that have been compared}
\item{Range}{The range of similarity coefficients}
\item{Median Similarity Coefficient}{The median similarity coefficient}
\item{Mean Similarity Coefficient}{The mean similarity coefficient}
\item{SEM of Similarity Coefficient}{The standard error of the mean similarity coefficient}
\item{The similarity coefficients}{a list of the similarity coefficients}
\item{Summary}{A table showing the pairs of runs that were compared (matches up with the list of similarity coeffiencts).} 
 
}
\references{
Rebbeck, CA, Jones, OR, & Tsai, IJ  (Submitted to \emph{Bioinformatics}) SimCo: A program to automate the comparison of multiple structure runs (available from \email{owen.jones@imperial.ac.uk})

Pritchard, J. K. et al. (2000) Inference of population structure using multilocus genotype data. \emph{Genetics}. 155, 945-959.


}
\author{Owen R. Jones (\email{owen.jones@imperial.ac.uk})}
\note{ 

 
}
\seealso{
\code{\link{SimCoImport}}
\code{\link{structureexample}}

}
\examples{

#You should import the files you want to use first using the SimCoImport() command.
#In this example it is assumed that you are using a pre-imported dataset called "structureexample"

data(structureexample)
SimCoef(structureexample)

}
\keyword{package}