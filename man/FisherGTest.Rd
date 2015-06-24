\name{FisherGTest}
\alias{FisherGTest}
\title{ Fisher's g test }
\description{
Calculates p-value using Fisher's g-test for a single frequency
}
\usage{
FisherGTest(z)
}
\arguments{
  \item{z}{ input series, no missing values }
}
\details{

}
\value{
p-value
}

\references{ 
Fisher, R.A. (1929). Tests of significance in harmonic analysis. Proc. Roy. Soc. A, 125, 54-59.
 }

\author{ A.I. McLeod }

\seealso{ \code{\link{PeriodicityTest}} }

\examples{
#Fourier Frequency
z<-SimulateHReg(10, f=2/10, 1, 2)
FisherGTest(z)
#non-Fourier Frequency
z<-SimulateHReg(10, f=2.5/10, 1, 2)
FisherGTest(z)
}

\keyword{ ts }
