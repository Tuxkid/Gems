logit <-
structure(function(p)
{
    pres <- (p > 0) & (p < 1)
    p[pres] <- log(p[pres]/(1 - p[pres]))
    n <- length(p[!pres])
    p[!pres] <- rep(NA, n)
    p
}
, comment = "28/03/2001")
