s.tapply <-
structure(function(x, y, z, ...)
{
# Counteracts tapply's propensity to return in sorted index order
#  instead of the order in which they were
    tapply(x, y, z, ...)[(unique(y))]
}
, comment = "30/04/2001")
