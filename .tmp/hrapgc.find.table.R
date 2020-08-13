find.table <-
structure(function(x, tab)
{
  ## Purpose: finds occurrence of x in tab and returns corresponding value
  ## ----------------------------------------------------------------------
  ## Modified from: old function called lookup
  ## ----------------------------------------------------------------------
  ## Arguments: 
  ## ----------------------------------------------------------------------
  ## Author: zaphod, Date: 21 Oct 2007, 21:24 
  if(dim(tab)[2] != 2)
    stop("tab must be a two column matrix\n")
  z <- tab[, 2]
  y <- tab[, 1]
    names(z) <- paste(y)
  z[paste(x)]  
}
, comment = "02/11/2007")
