time.to.mort <-
structure(function(ab, link = "cloglog", pc = 0.99996830000000003)
{
### For calculating time to (by default) Probit9 mortality, or any other %
#
### Note: if the x-axis has used a transformation before fitting the line, 
###  the reverse transformation must be made to this output.
#
    f <- link.function(link)
    targ <- f(ab$cm + (1 - ab$cm) * pc)
    lt <- (targ - ab$intercept)/ab$slope
    lt
}
, comment = "25/04/1997")
