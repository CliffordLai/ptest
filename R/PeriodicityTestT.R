`PeriodicityTestT` <-
function(x, NSim=10^4){
#
#This is for individual series
#
# Input Parameters:
# x - data 
# nsim - number of random series to be generated
# 
# NSim - number of simulations
# obsTimes - observations numbers corresponding to observed values
    n <- length(x)
    i <- 1:n
    obsTimes <- i[!is.na(x)]
    ObsLLR <- GetFitHReg(x[obsTimes], t=obsTimes)[1]
    xm <- x[obsTimes]
    m <- length(obsTimes)
    llr <- numeric(NSim)
    for (isim in 1:NSim){
        zsim <- rt(m, 5)
        llr[isim] <- GetFitHReg(zsim, obsTimes)[1]
    }
    pvalue <- (sum(llr >= ObsLLR)+1)/(NSim+1)
    pvalue
}

