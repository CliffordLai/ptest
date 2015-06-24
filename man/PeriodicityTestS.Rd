\name{PeriodicityTestS}
\alias{PeriodicityTestS}
\title{ TestS a series for periodicity }
\description{
TestS for periodicity. Missing values allowed.
}
\usage{
PeriodicityTestS(x, NSim = 10^4)
}

\arguments{
  \item{x}{ series }
  \item{NSim}{ number of Monte-Carlo iterations }
}
\details{
t(5,5) series of the same length as the original are used.
Skewed error case.
}
\value{
p-value
}
\references{ }
\author{ A.I. McLeod }

\seealso{ 
\code{\link{FisherGTest}}, 
\code{\link{PeriodicityTest}}, 
\code{\link{PeriodicityTestT}} 
}

\examples{
z<-SimulateHReg(10, f=2.5/10, 1, 2, Dist="s")
PeriodicityTestS(z)

}

\keyword{ ts }
