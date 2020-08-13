blank.plot <-
structure(function(x = 0, y = 0, ...)
{
# Does a blank plot.  
#
#  Useful if you want to change the axes on the current plot (in which case specify
#    par(new=F)) 
# Sometimes you might want to prevent plotting in one part of a multiplot page.
    plot(x, y, xlab = "", ylab = "", xaxt = "n", yaxt = "n", pch = " ", bty = 
       "n", ...)
}
, comment = "06/05/1997")
