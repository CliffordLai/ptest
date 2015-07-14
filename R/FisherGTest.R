`FisherGTest` <-
function(z){
    n <- length(z)
    m <- ifelse(n%%2==0,(n-2)/2,(n-1)/2)
    Ip <- pgram(z)
    if( n%%2 ==0 )
        Ip<-Ip[-(m+1)]
    g <- max(Ip)/sum(Ip)
    p <- floor(1/g)
    i <- 1:p
    sum(choose(m,i)*(-1)^(i-1) *(1-i*g)^(m-1))
 }

