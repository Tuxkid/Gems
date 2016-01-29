stdev <-
structure(function(x, na.rm = F)
{
    if(na.rm)
       x <- x[!is.na(x)]
    if(length(x) == 0)
       NA
    else sqrt(var(x))
}
, comment = "21/09/1998")
