###########################
# Compute the estimated p-value of the observed statistics
# Ti: the length of the series 
# Statistic: the computed observed statistics
# modelList: the RSR fitted models for the required statistic
PvalueRSR <- function(Ti,Statistic,modelList){ 
  alpha <- modelList$alpha
  
  qa_est <- PredictRSR(Ti,modelList)
  
  CDF <- approxfun(x = c(0,qa_est,Inf), y = c(0,alpha,1), method = "linear")
  
  pvalue <- 1-CDF(Statistic)
  pvalue
}

###########################
# Compute the estimated quantiles of the required statistics
#Input:
# Ti: the length of the series 
# modelList: the RSR fitted models for the required statistics

#Output: 
# All estimated quantiles for Ti
PredictRSR <- function(Ti,modelList){
  f <- modelList$f
  model_matrix <- modelList$model_matrix
  Intercept <- modelList$Intercept
  
  if(Intercept){
    New <- rep(1, length(Ti))
    p <- ncol(model_matrix)-1
  }else{
    New <- NULL
    p <- ncol(model_matrix)
  }
  
  I1 <- 1/(f(Ti))
  
  for(i in 1:p){
    New <- rbind( New, I1^(i*0.5+0.5) )
  }
  
  model_matrix%*%New
} 
