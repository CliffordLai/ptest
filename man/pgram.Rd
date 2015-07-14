\name{pgram}
\alias{pgram}
\title{ Periodogram computation }
\description{
The periodogram is computed.
}
\usage{
pgram(z)
}
\arguments{
  \item{z}{series }
}
\details{
Uses FFT.
So if the length of z is a highly composite number, the computation
is very efficient.
The periodogram is computed for the Fourier frequencies,
(1:nF)/n, where nF=floor(n/2), n=length(z).
}

\value{
Periodogram evaluated at the Fourier frequencies
}


\references{ }

\author{ A.I. McLeod }

\seealso{ \code{\link{help}} }
\examples{
z<-sunspot.year
n<-length(z)
I<-pgram(z)
nF<-floor(n/2)
f<-(1:nF)/n
plot(f, I, xlab="f", ylab="f", type="l") 
title(main="Periodogram for Annual Sunpots, 1700-1988")
}
\keyword{ ts }
