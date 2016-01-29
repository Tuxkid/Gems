rms <-
structure(function(x, na.rm = T, zero.rm = F)
{
    if(na.rm)
       x <- x[!is.na(x)]
    if(zero.rm)
       x <- x[abs(x) > 0]
    sqrt(mean(x^2))
}
, comment = "30/11/1996")
