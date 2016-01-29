ipsofactor <-
structure(function(x)
{
  ## Purpose:  Makes a factor with levels in the current order
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##           x: character vector 
  ## ----------------------------------------------------------------------
  ## Author: Zaphod Beeblebrox, Date:  6 Mar 2002, 10:47

y <- factor(x, levels = unique(x))
y
}
, comment = "28/06/2002")
