pdfn <-
structure(function(x, ps2pdf = TRUE, cex = .5, adj = 1, side = 1, line = -1, ...)
{
### Purpose: Print file name on a plot
### ----------------------------------------------------------------------
### Modified from: 
### ----------------------------------------------------------------------
### Arguments: ps2pdf -- are we making a postscript name into a PDF name?
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 24 Feb 2004, 14:55
  par(new = T)
  par(omi = rep(0,4), mai = rep(0,4), ...)
  if(ps2pdf)
    x <- ppaste(substring(x, 1,nchar(x)-1), "df")
  blank.plot()
  mtext(x, side = side, line = line, cex = cex, adj = adj)
}
, comment = "28/07/2004")
