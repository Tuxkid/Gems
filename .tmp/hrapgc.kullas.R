kullas <-  function(bits = 1:11, ad.tex.cex = .9)
{
### Purpose: Basic Trellis colours for lattice plots
### ----------------------------------------------------------------------
### Modified from: col.spray in /home/hrapgc/Rstuff/garry/industry
### ----------------------------------------------------------------------
### Arguments:
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 16 Jan 2004, 13:52
### ----------------------------------------------------------------------
### Revisions:- 13/3/15 fontsize added: it allows pointsize to be not ignored

  lab.cex <- 1.2
  add.text <- trellis.par.get("add.text")
  add.text$cex <- ad.tex.cex
  require("RColorBrewer")
  use.colours <- brewer.pal(8, "Dark2")
  list(background = list(col="transparent"),
       bar.fill = list(col="#c8ffc8"),
       box.rectangle = list(col = "darkgreen"),
       box.umbrella = list(col = "darkseagreen", lty = 1),
       box.dot = list(alpha = 1, col = "darkorchid", cex = 1),
       dot.line = list(col="#e8e8e8"),
       dot.symbol = list(col="orchid", cex = .8, pch = 1),
       plot.line = list(col="darkgreen"),
### This one sets alpha for dot.symbol also -- overides its alpha
       plot.symbol = list(alpha = 1, col="orchid", cex = .8, pch = 1),
       plot.polygon = list(border = "goldenrod", col="goldenrod3", lwd = 2),
       par.xlab.text = list(cex = lab.cex),
       par.ylab.text = list(cex = lab.cex),
       add.text = add.text,
       fontsize = list(text = NULL),
       axis.components = list(
         left = list(tck = .6, pad1 = .5),
         top = list(tck = .6, pad1 = .65),
         right = list(tck = .6, pad1 = .5),
         bottom = list(tck = .6, pad1 = .65)),

       regions = list(col = rev(heat.colors(100))),
       reference.line = list(col="turquoise", lty = 3),
       superpose.line = list(col = use.colours,
         lty = rep(1,11), lwd = rep(1, 11)[bits]),
       superpose.symbol = list(pch = bits, cex = rep(.9, length(bits)),
         col = use.colours)
       ) 
}
