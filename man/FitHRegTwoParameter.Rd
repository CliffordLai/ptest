\name{FitHRegTwoParameter}
\alias{FitHRegTwoParameter}
\title{ Fit Two Parameter Harmonic Regression }
\description{
Given the frequency, f, we fit the regression
}
\usage{
FitHRegTwoParameter(y, t = 1:length(y), f = 1/length(y))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ series }
  \item{t}{ time points }
  \item{f}{ known frequency }
}
\details{
Uses lm.
}
\value{
object of class "HReg" produced
}
\references{ }
\author{A.I. McLeod }

\seealso{  \code{\link{FitHReg}} }
\examples{
z<-SimulateHReg(10, f=2.5/10, 1, 2)
FitHRegTwoParameter(z, f=2.5/10)
}
\keyword{ ts }
