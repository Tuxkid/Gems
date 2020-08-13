disp <-
structure(function(x, r = 4)
{
### Purpose: Estimates dispersion of a glm
### ----------------------------------------------------------------------
### Modified from: 
### ----------------------------------------------------------------------
### Arguments: x is a glm object (mabye can be more generic)
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date:  8 Aug 2003, 11:21

  dev.x <- deviance(x)
  df.x <- x$df.residual
  round(dev.x/df.x, r)
}
, comment = "08/08/2003")
