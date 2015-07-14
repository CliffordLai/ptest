#These programs are from the R package "GeneCycle", but they are modified a little bit for simplification.

#Compute the Robust g test statistic 
`GetFitRankG` <- function(y){ 
  Dpgram <- Rankspectrum(y)
  
  max(Dpgram[-1])/sum(Dpgram[-1])
}


#################################################################
# This function implements the robust, rank-based spectral#######
# estimator introduced in Pearson et al. 2003####################
#################################################################
Rankspectrum <- function(x) 
{
  ##############################################
  # Some adjustable parameters
  ##############################################
  # Length of the zero-padded one-sided "rho"(=Rsm)
  zp <- 2*length(x)
  
  # Length of the original sequence
  n <- length(x)
  
  # Let us define the maximum lag for the correlation coefficient:
  maxM <- n-2
  
  ##############################################
  # Correlation coefficient
  ##############################################
  # Reserve space
  Rsm <- matrix(NA, nrow = maxM+1, ncol = 1)
  nonMissing <- rep(TRUE,n)
  nmi <- 1:n # indices for nonmissing values
  
  # Mean removal
  x<- x- mean(x)
  
  # Run through all the lags
  for (lags in 0:maxM)
  {
    # Modified Spearman's method
    indexes <- 1:(n-lags)	# Initial indices
    ends <- n
    
    # Values in both the original and shifted vectors must be present:
    temp <- (nonMissing[1:(ends-lags)] + nonMissing[(lags+1):ends]) >= 2
    indexPresent <- which(temp)
    indexes <- indexes[indexPresent]	# The indices that are present in
    # both sequences
    Rsm[lags+1] <- ifelse(
      length(indexes)<=1 , 
      0 ,
      cor(x[indexes], x[indexes+lags], method="spearman" ) * length( x[indexes] )/n )   # Spearman's correlation coefficient
  }
  
  # Zero-padding
  Rsm[(length(Rsm)+1):zp] <- 0
  fftemp <- fft(Rsm)
  
  # The following implementation is as in (Ahdesmäki, Lähdesmäki et al., 2005)
  Ssm <- abs( 2*Re(fftemp) - Rsm[1] )
  Ssm <- Ssm[1:floor(length(Ssm)/2)]
  
  # Return the spectral content, frequencies [0,pi)
  return(Ssm)
}

