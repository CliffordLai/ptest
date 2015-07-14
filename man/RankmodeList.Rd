\name{RankmodeList}
\alias{RankmodeList}
\docType{data}
\title{ The result for the RSR method with respect to the robust g test}
\description{
This data set is used to build up the response surface regressions in order to obtain the p-values for the robust g test.
}
\usage{data(RankmodeList)}

\format{
    A list object with the following components:
  
  \code{model_matrix}: A matrix with the estimated coefficents for the Response Surface Regressions (one row per quantile). 
  
  \code{f}: A function determine the rate of convergence for the Response Surface Regressions. 
  
  \code{alpha}: A vector containing all positions of the estimated quantiles. 
  
  \code{Intercept}: A bool value indicates that whether the intercept term is included in the Response Surface Regressions. 
}

\details{

}
\source{
}
\references{
MacKinnon, James (2001) : Computing numerical distribution functions in 
 econometrics, Queen's Economics Department Working Paper, No. 1037.
}

\examples{


}
\keyword{datasets}

