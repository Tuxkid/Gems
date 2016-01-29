sem <-
structure(function(x, na.rm = F)
{
    if(na.rm)
       x <- x[!is.na(x)]
    if(length(x) == 0)
       NA
    else sqrt(var(x)/length(x))
}
, comment = "09/09/1995")
