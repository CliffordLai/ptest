`GetFitHReg` <-
function (y, t = 1:length(y)) 
{
    theta <- numeric(2)
    theta[1] <- length(y)
    ans <- .C("GetHReg", y = as.double(y), t = as.double(t), 
        theta = as.double(theta), PACKAGE = "ptest")
    ans$theta
}
