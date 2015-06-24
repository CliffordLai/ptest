`FitHRegTwoParameter` <-
function (y, t = 1:length(y), f=1/length(y)) 
{
    x1<-cos(2*pi*f*t)
    x2<-sin(2*pi*f*t)
    ansLM<-lm(y~x1+x2)
    co<-coef(ansLM)
    names(co)<-c("mu","A","B")
    a<-summary(ansLM)
    v<-a$cov.unscaled
    Rsq<-a$r.squared
    fstat<- a$fstatistic
    sig<-a$sigma
    res<-residuals(ansLM)
    n<-length(y)
    SSTot <- sum((y-mean(y))^2)
    SSRes <- sum(res^2)
    LR <- (SSTot/SSRes)^(n/2)
    ans<-list(coefficients=co, residuals=res, Rsq=Rsq, fstatistic=fstat, sigma=sig, freq=f, LRStat=LR)
    class(ans)<-"HReg"
    ans
}

