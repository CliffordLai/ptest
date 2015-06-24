\name{PeriodicityTest}
\alias{PeriodicityTest}
\title{ Test a series for periodicity }
\description{
Test for periodicity. Missing values allowed.
}
\usage{
PeriodicityTest(x, NSim = 10^4, seed=321, ignoreLowQ=FALSE)
}

\arguments{
  \item{x}{ series }
  \item{NSim}{ number of Monte-Carlo iterations }
  \item{seed}{ initial seed} 
  \item{ignoreLowQ}{ set TRUE to ignore frequencies 
  whose periods correspond to periods less than the observed series
  length. }
}
\details{
NID(0,1) series of the same length as the original are used.
}

\value{
p-value.
Is set to NA when ignoreLowQ=TRUE and the estimated frequency
corresonds to a period less than the observed series length.
}

\references{ 

}

\author{ A.I. McLeod }

\seealso{ \code{\link{FisherGTest}} }

\examples{
z<-SimulateHReg(10, f=2.5/10, 1, 2)
PeriodicityTest(z)

}

\keyword{ ts }
