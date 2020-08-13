axis.fudge <-
structure(function(ax.cex = .8, las = 2, y.tix, mgp = c(2, .7, 0), fudge = 0)
{
  ## Purpose: Fudges y-axis labels (if the default is too low)
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: y.ax.fudge is the amount to bump up the position of
  ##            ytick labels 
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 17 Jun 2004, 13:44

  axis(2, cex.axis = ax.cex, mgp = mgp, at = y.tix, labels = FALSE)
  axis(2, cex.axis = ax.cex, las = 2, mgp = mgp,
       at = y.tix + fudge, labels = paste(y.tix), tick = FALSE)

}
, comment = "18/06/2004")
