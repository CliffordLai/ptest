\name{GetFitModifiedFisherG}
\alias{GetFitModifiedFisherG}

\title{Compute the extended Fisher's G test statistic }

\description{
The extended Fisher's G test statistic is computed for testing for periodicity.
}

\usage{
GetFitModifiedFisherG(y)
}
\arguments{
  \item{y}{ vector containing the series. }
}
\details{
Extend the Fisher's g test by enlarging twice the searching region of the fourier frequencies.
}
\value{
The extended Fisher's G test statistic.

}

\references{
Fisher, R.A. (1929). Tests of significance in harmonic analysis. Proc. Roy. Soc. A, 125, 54-59.
}
\author{ Yuanhao Lai.  }


\examples{
}

\keyword{ ts }
