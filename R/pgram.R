`pgram` <-
function(z)
{
    n <- length(z)
    2 * (Mod(fft(z))^2/n)[2:(n %/% 2 + 1)]
}

