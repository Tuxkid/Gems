average.down <-
structure(function(x)
{
# uses fill.down() to average the missing values with ones present
    y <- fill.down(x)
    z <- rev(fill.down(rev(x)))
    xx <- (y + z)/2
}
, comment = "10/08/1999")
