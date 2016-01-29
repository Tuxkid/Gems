lat.set <-
structure(function(setting = par.xlab.text, subsetting = cex, to = 1.3)
{
  ## Purpose: Change trellis settings in one move
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##  setting    -- which setting is to be changed
  ##                  (a name of a list from trellis.settings)
  ##  subsetting -- which part of that list
  ##  to         -- what are we changing it to
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Date: 19 Apr 2002, 10:06

  y <- as.character(substitute(setting))
  z <- as.character(substitute(subsetting))
  x <- trellis.par.get(y)
  x[[z]] <- to
  trellis.par.set(y, x)
}
, comment = "07/06/2002")
