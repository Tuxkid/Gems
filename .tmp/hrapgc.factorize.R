factorize <-
structure(function(n)
{
    p <- n/(z <- 1:ceiling(sqrt(n)))
    z <- z[trunc(p) == p]
    unique(c(z, rev(n/z)))
}
, comment = "14/12/1998")
