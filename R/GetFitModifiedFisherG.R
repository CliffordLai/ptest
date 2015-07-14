`GetFitModifiedFisherG` <- function(y) {
  #Compute the extended Fisher's G test statistic 
  n <- length(y)
  z <- c(y,rep(0,n))
  
  if( n%%2 ==0 ){
    Dpgram <- ( Mod(fft(z))^2 )[3:(n-1)] /n
  }else{
    Dpgram <- ( Mod(fft(z))^2 )[3:n] /n
  }
  
  max(Dpgram)/sum(Dpgram)
}