fill.down <-
structure(function(x)
{
# Replaces NAs in a numeric vector or blanks in a character one 
#   with the value preceding it.  
#    Too lazy to use fill-down in Excel
#
#  Warning: Replaces an initial NA with a zero.  Might not always be appropriate.
if(is.factor(x))
  x <- as.character(x)
  if(is.na(x[1])) x[1] <- 0
    stillsome <- T
    while(stillsome) {
       nas.at <- iff(is.character(x), x == "", is.na(x))
       na.indx <- seq(along = x)[nas.at]
       x[na.indx] <- x[na.indx - 1]
       stillsome <- length(na.indx) > 0
    }
    x
}
, comment = "27/05/2009")
