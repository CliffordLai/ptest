\name{FitHReg}
\alias{FitHReg}
\title{ Fits Three Parameter Harmonic Regression }
\description{
Estimates A, B and f in the harmonic regression,
y(t)=mu+A*cos(2*pi*f*t)+B*sin(2*pi*f*t)+e(t)
using LS.
}
\usage{
FitHReg(y, t = 1:length(y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ series }
  \item{t}{ time points }
}
\details{
Program is interfaced to C for efficient computation.
}
\value{
object of class "HReg" produced.
}
\references{  }
\author{ A.I. McLeod }

\seealso{ \code{\link{GetFitHReg}} }

\examples{
z<-SimulateHReg(10, f=2.5/10, 1, 2)
FitHReg(z)
}
\keyword{ ts }
