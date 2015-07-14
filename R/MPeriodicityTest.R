`MPeriodicityTest` <- function(X, NSim=10^4, seed=321,
                               method = c("LR","FisherG","ModifiedFisherG","RankRSR","LRRSR","ModifiedFisherG50RSR")){
  #This is for multiple series with same lengths but it is not valid for series containing missing values
  #
  # Input Parameters:
  # X - a matrix consisting of the time series as column vectors
  # NSim - number of random series to be generated
  # seed -- initialize random seed
  # method --
         # LR -- Use the likelihood ratio test based on the four-parameter harmonic regression, where the p-values are computed by Monte Carlo simulations.
         # FisherG -- Use the Fisher's G exact test.
         # ModifiedFisherG -- Extend the Fisher's g test by enlarging twice the searching region of the fourier frequencies, where the p-value is computed by Monte Carlo simulations. 
         # RankRSR -- Use the Robust g test proprosed in Ahdesmaki et al. (2005), where the p-values are computed by the Response Surface Regression proprosed by Mackinnon(2002).
         # LRRSR -- Same test as "LR" but its p-values are computed by the Response Surface Regression proprosed by Mackinnon(2002).
         # ModifiedFisherGRSR -- Same test as "ModifiedFisherG" but it is only applicable if the sample size is not less than 50, where the p-values are computed by the Response Surface Regression proprosed by Mackinnon(2002).
  
  if(!( is.matrix(X) || is.data.frame(X) ) ) stop("the input x should be either a matrix or a data.frame!")
  if( any( is.na(X) ) ) stop("Data contain missing values!")
  
  n <- nrow(X)
  m <- ncol(X)
  
  method <- match.arg(method)
  
  if(method == "LR"){                                    #Use the likelihood ratio test
  
    set.seed(seed)
    Statistic <- numeric(NSim)
    pvalue <- numeric(m)
    
    for(isim in 1:NSim){
      zsim <- rnorm(n)
      Statistic[isim] <- GetFitHReg(zsim)[1]
    }
    
    for(i in 1:m){
      ans <- GetFitHReg(X[,i])
      ObsStatistic <- ans[1]
      pvalue[i] <- (sum(Statistic >= ObsStatistic)+1)/(NSim+1)
    }
    return(pvalue)
    
  }
  else if(method == "FisherG"){                         #Use the Fisher's G test
    
    pvalue <- apply(X,MARGIN = 2,FUN = FisherGTest )
    return(pvalue)
    
  }
  else if(method == "ModifiedFisherG"){                 #Use the extended Fisher's G test
    
    set.seed(seed)
    Statistic <- numeric(NSim)
    pvalue <- numeric(m)
    
    for(isim in 1:NSim){
      zsim <- rnorm(n)
      Statistic[isim] <- GetFitModifiedFisherG(zsim)
    }
    
    for(i in 1:m){
      ans <- GetFitModifiedFisherG(X[,i])
      ObsStatistic <- ans[1]
      pvalue[i] <- (sum(Statistic >= ObsStatistic)+1)/(NSim+1)
    }
    return(pvalue)
    
  }
  else if(method == "RankRSR"){                        #RSR method for the robust g test
    
    Statistic <- numeric(m)
    
    for(i in 1:m){
      Statistic[i] <- GetFitRankG(X[,i])
    }
    
    data("RankmodeList",package = "ptest")
    print("The Response Surface for the the robust g test is loaded!")
    pvalue <- PvalueRSR(Ti = n,Statistic = Statistic, modelList = RankmodeList)
    return(pvalue)
    
  }
  else if(method == "LRRSR"){                         #RSR method for the likelihood ratio test
    
    Statistic <- numeric(m)
    
    for(i in 1:m){
      Statistic[i] <- GetFitHReg(X[,i])[1] 
    }

    data("LLRmodeList",package = "ptest")
    print("The Response Surface for the likelihood ratio test is loaded!")
    pvalue <- PvalueRSR(Ti = n,Statistic = Statistic, modelList = LLRmodeList)
    return(pvalue)
    
  }
  else if(method == "ModifiedFisherG50RSR"){         #RSR method for the extended Fisher's G test
    
    if(n < 50){
      stop("The RSR method for the extended Fisher's G test requires the sample length not less than 50")
    }
    
    Statistic <- numeric(m)
    
    for(i in 1:m){
      Statistic[i] <- GetFitModifiedFisherG(X[,i])
    }
    
    data("FisherGmodeList50",package = "ptest")
    print("The Response Surface for the extended Fisher's G test is loaded!")
    pvalue <- PvalueRSR(Ti = n,Statistic = Statistic, modelList = FisherGmodeList50)
    return(pvalue)
 }

}

