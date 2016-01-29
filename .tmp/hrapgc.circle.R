circle <-
structure(function(cen.x = 0, cen.y = 0, rad = 1, from = 0, to = 360, seg = 400, ...)
{
  ## Purpose: draws a circle (or part thereof) at cen.x, cen.y, with radius rad
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: 
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date:  3 Feb 2006, 16:08

  bits <- seq(from, to, length = 400)
  x.coords <- cen.x + rad * cos (bits * pi/180)
  y.coords <- cen.y + rad * sin (bits * pi/180)
  lines(x.coords, y.coords, ...)
}
, comment = "29/03/2006")
