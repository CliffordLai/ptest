\name{ptest-package}
\alias{ptest-package}
\alias{ptest}
\docType{package}
\title{
Test for periodicity in short time series
}
\description{
This is a complete package for testing a large number of short
time series for periodicity.
}
\details{
\tabular{ll}{
Package: \tab ptest\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-08-09\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

}

\author{
A.I. McLeod and M.S. Islam

<aimcleod@uwo.ca>

}
\references{
Islam, M.S. (2008).  
Peridocity, Change Detection and Prediction in Microarrays. 
Ph.D. Thesis, The University of Western Ontario. 
}

\keyword{ package }

\seealso{

}

\examples{
z<-SimulateHReg(10, f=2.5/10, 1, 2)
PeriodicityTest(z)
}
