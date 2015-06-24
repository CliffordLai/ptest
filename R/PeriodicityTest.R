`PeriodicityTest` <-
function(x, NSim=10^4, seed=321, ignoreLowQ=FALSE){
#
#This is for individual series
#
# Input Parameters:
# x - data 
# nsim - number of random series to be generated
# seed -- initialize random seed
# 
# NSim - number of simulations
# obsTimes - observations numbers corresponding to observed values
    set.seed(seed)
    n <- length(x)
    i <- 1:n
    obsTimes <- i[!is.na(x)]
    ans <- GetFitHReg(x[obsTimes], t=obsTimes)
    if (ignoreLowQ && ans[2] < 1/max(obsTimes))
    	pvalue<-NA
    else
    	{
    	ObsLLR <- ans[1]
    	xm <- x[obsTimes]
    	m <- length(obsTimes)
    	llr <- numeric(NSim)
    	for (isim in 1:NSim){
        	zsim <- rnorm(m)
        	llr[isim] <- GetFitHReg(zsim, obsTimes)[1]
    	}
    	pvalue <- (sum(llr >= ObsLLR)+1)/(NSim+1)
	}
    pvalue
}

