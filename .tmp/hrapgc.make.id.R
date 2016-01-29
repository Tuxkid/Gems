make.id <-
structure(function(x)
{
# Creates one id value for each row.  Value increases by 1 for each
# element where the value of x is less than for the previous element.
    dx <- c(0, diff(x))
    u <- rep(0, length(x))
    u[dx < 0] <- 1
    cumsum(u) + 1
}
, comment = "07/04/1997")
