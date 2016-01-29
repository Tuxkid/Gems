lerrorbars <-
structure(function(x, y, se, yl = y - se, yu = y + se, eps, ...)
{
  ## Purpose: Lattice errorbars
  ## ----------------------------------------------------------------------
  ## Modified from: errorbar
  ## ----------------------------------------------------------------------
  ## Arguments: 
  ## ----------------------------------------------------------------------
  ## Author: Marcus Davy , Creation date: 31 Aug 2005, 10:58

  if(any(is.na(yl)) | any(is.na(yu)))
    ltext(x[is.na(yl) | is.na(yu)], y[is.na(yl) | is.na(yu)], "NA",
          adj = 1)
  lsegments(x, yl, x, yu, ...)
  lsegments(x - eps, yl, x + eps, yl, ...)
  lsegments(x - eps, yu, x + eps, yu, ...)
}
, comment = "31/08/2005")
