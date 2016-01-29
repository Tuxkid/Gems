circ.boxplot <-
structure(function(x, rad, fat = .1, col = "blue", rad.lim = 1,
           pch = 16, cex = .8, seg = 600, test = FALSE)
{
  ## Purpose: Curved boxplots
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: x- vector of values (in degrees),
  ##            rad- how far from centre to draw
  ##            fat- how thick the boxes are to be
  ##            rad.lim- max radial length on plot
  ##            test(mark individual values to test the box position)
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 29 Mar 2006, 13:20
  deg <- boxplot(x, plot = FALSE)
  box.stats <- deg$stats[,1]
  names(box.stats) <- c("beginWhisker", "beginBox", "Median", "endBox", "endWhisker")
  rad <- rad/rad.lim
  fat <- fat/rad.lim # get to unit scale if not already
### draw inner and outer sides of boxes and the whiskers
  circle(,, rad + fat/2, box.stats["beginBox"], box.stats["endBox"], col = col, seg = seg)
  circle(,, rad - fat/2, box.stats["beginBox"], box.stats["endBox"], col = col, seg = seg)
  circle(,, rad , box.stats["beginWhisker"], box.stats["beginBox"], col = col, seg = seg)
  circle(,, rad , box.stats["endBox"], box.stats["endWhisker"], col = col, seg = seg)
### co-ordinates for box edges
  x.box.i <- (rad - fat/2)* cos(box.stats * pi/180)[c("beginBox", "endBox")]
  y.box.i <- (rad - fat/2) * sin(box.stats * pi/180)[c("beginBox", "endBox")]
  x.box.o <- (rad + fat/2)* cos(box.stats * pi/180)[c("beginBox", "endBox")]
  y.box.o <- (rad + fat/2) * sin(box.stats * pi/180)[c("beginBox", "endBox")]
  ## draw two radial lines for ends of boxes
  lines(c(x.box.i["beginBox"], x.box.o["beginBox"]),
        c(y.box.i["beginBox"], y.box.o["beginBox"]), col = col)
  lines(c(x.box.i["endBox"], x.box.o["endBox"]),
        c(y.box.i["endBox"], y.box.o["endBox"]), col = col)
### co-ordinates for whisker edges
  x.whisker.i <- (rad - fat/4)*
    cos(box.stats * pi/180)[c("beginWhisker", "endWhisker")]
  y.whisker.i <- (rad - fat/4) *
    sin(box.stats * pi/180)[c("beginWhisker", "endWhisker")]
  x.whisker.o <- (rad + fat/4)*
    cos(box.stats * pi/180)[c("beginWhisker", "endWhisker")]
  y.whisker.o <- (rad + fat/4) *
    sin(box.stats * pi/180)[c("beginWhisker", "endWhisker")]
  ## draw two radial lines for ends of whiskers
  lines(c(x.whisker.i["beginWhisker"], x.whisker.o["beginWhisker"]),
        c(y.whisker.i["beginWhisker"], y.whisker.o["beginWhisker"]), col = col)
  lines(c(x.whisker.i["endWhisker"], x.whisker.o["endWhisker"]),
        c(y.whisker.i["endWhisker"], y.whisker.o["endWhisker"]), col = col)
### co-ordinates for median edges
  x.median.i <- (rad - fat/2) * cos(box.stats[c("Median")] * pi/180)
  y.median.i <- (rad - fat/2) * sin(box.stats[c("Median")] * pi/180)
  x.median.o <- (rad + fat/2) * cos(box.stats[c("Median")] * pi/180)
  y.median.o <- (rad + fat/2) * sin(box.stats[c("Median")] * pi/180)
  ## draw  radial line for median
  lines(c(x.median.i["Median"], x.median.o["Median"]),
        c(y.median.i["Median"], y.median.o["Median"]), lwd = 3, col = col)
### Mark in outliers
  x.out <- rad * cos(deg$out * pi/180)
  y.out <- rad * sin(deg$out * pi/180)
  points(x.out, y.out, pch = pch, cex = cex, col = col)
  if(test){ # if we're testing to see if the box is in the right quadrant
    points(rad * cos(x * pi/180), rad * sin(x * pi/180),
           pch = 1, col = col, cex = cex )
  }
}
, comment = "02/11/2006")
