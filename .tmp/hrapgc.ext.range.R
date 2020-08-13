ext.range <-
structure(function(x, a = 5, ...)
{
  ## Purpose: Extend the xlim or ylim range of a vector
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: a is % to extend the range of vector x
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 13 Jan 2006, 09:11
ra <- range(x, ...)
interval <- diff(ra)
adjustment <- interval * a/100
ra + adjustment * c(-1, 1)
}
, comment = "13/01/2006")
