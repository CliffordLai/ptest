\name{MPeriodicityTest}
\alias{MPeriodicityTest}
\title{ Test multiple series for periodicity }
\description{
This function is used to test multiple series with same lengths for the periodicity but it is not valid for series containing missing values.
}
\usage{
MPeriodicityTest(X, NSim=10^4, seed=321,
  method = c("LR","FisherG","ModifiedFisherG","RankRSR","LRRSR","ModifiedFisherG50RSR"))
}

\arguments{
  \item{X}{ a matrix consisting of the time series as column vectors }
  \item{NSim}{ the number of Monte-Carlo simulations. }
  \item{seed}{ the initial seed for Monte-Carlo simulations. } 
  \item{method}{ the test to be performed. See details for more information. } 
}
\details{
NID(0,1) series of the same length as the original are used for Monte-Carlo simulations.

Discriptions of different methods:

  \code{LR}: Use the likelihood ratio test based on the four-parameter harmonic regression, where the p-values are computed by Monte Carlo simulations.
  
  \code{FisherG}: Use the Fisher's G exact test.
  
  \code{ModifiedFisherG}: Extend the Fisher's g test by enlarging twice the searching region of the fourier frequencies, where the p-value is computed by Monte Carlo simulations. 
  
  \code{RankRSR}: Use the Robust g test proprosed in Ahdesmaki et al. (2005), where the p-values are computed by the Response Surface Regression proprosed by Mackinnon(2002).
  
  \code{LRRSR}: Same test as "LR" but its p-values are computed by the Response Surface Regression proprosed by Mackinnon(2002).
  
  \code{ModifiedFisherG50RSR}: Same test as "ModifiedFisherG" but it is only applicable if the sample size is not less than 50, where the p-values are computed by the Response Surface Regression proprosed by Mackinnon(2002).
}

\value{
A vector of p-values.
}

\references{ 
Fisher, R.A. (1929). Tests of significance in harmonic analysis. Proc. Roy. Soc. A, 125, 54-59.

Ahdesmaki, M., Lahdesmaki, H., Pearson, R., Huttunen, H., and 
Yli-Harja O.
  (2005).  \emph{BMC Bioinformatics} \bold{6}:117. \url{http://www.biomedcentral.com/1471-2105/6/117}
}

\author{ Yuanhao Lai. }

\seealso{ 
  \code{\link{FisherGTest}} 
  \code{\link{PeriodicityTest}}
}

\examples{
#Try the likelihood ratio test 
X <- replicate( 100,SimulateHReg(10, f=2.5/10, 1, 2) )
MPeriodicityTest(X, NSim=10^4, seed=321, method = "LR")

#Try the likelihood ratio test with response surface regressions
X <- replicate( 100,SimulateHReg(10, f=2.5/10, 1, 2) )
MPeriodicityTest(X,method = "LRRSR")
}

\keyword{ ts }
