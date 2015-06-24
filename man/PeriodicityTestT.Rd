\name{PeriodicityTestT}
\alias{PeriodicityTestT}
\title{ TestT a series for periodicity }
\description{
TestT for periodicity. Missing values allowed.
}
\usage{
PeriodicityTestT(x, NSim = 10^4)
}

\arguments{
  \item{x}{ series }
  \item{NSim}{ number of Monte-Carlo iterations }
}
\details{
t(5) series of the same length as the original are used.
Thick-tail error case.
}
\value{
p-value
}
\references{ }
\author{ A.I. McLeod }

\seealso{ 
\code{\link{FisherGTest}}, 
\code{\link{PeriodicityTest}}, 
\code{\link{PeriodicityTestS}} 
}

\examples{
z<-SimulateHReg(10, f=2.5/10, 1, 2, Dist="t")
PeriodicityTestT(z)

}

\keyword{ ts }

