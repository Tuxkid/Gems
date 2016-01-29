p.sunflowers <-
structure(function(x, y, number, size = 0.125, add = FALSE, rotate = F, pch = 16, ...)
{
## Purpose: Produce a 'sunflower'-Plot
## -------------------------------------------------------------------------
## Arguments: x,y: coordinates;
##    number[i] = number of times for (x[i],y[i])  [may be 0]
##    size: in inches  add : Should add to a previous plot ?
##    rotate: randomly rotate flowers? further args: as for plot(..)
## -------------------------------------------------------------------------
## Authors: Andreas Ruckstuhl, Werner Stahel, Martin Maechler, Tim Hesterberg
## Date   : Aug 89 / Jan 93,   March 92,      Jan, Dec 93,        Jan 93
## Examples: p.sunflowers(x=sort(2*round(rnorm(100))), y=round(rnorm(100),0))
## ~~~~~~~~  p.sunflowers(rnorm(100),rnorm(100),number=rpois(n=100,lambda=2), 
##                        rotate=T, main="Sunflower plot")
    n <- length(x)
    if(length(y) != n)
       stop("x & y must have same length !")
    if(missing(number)) {
       orderxy <- order(x, y)
       x <- x[orderxy]
       y <- y[orderxy]
       first <- c(T, (x[-1] != x[ - n]) | (y[-1] != y[ - n]))
       x <- x[first]
       y <- y[first]
       number <- diff(c((1:n)[first], n + 1))
    }
    else {
       if(length(number) != n)
          stop("number must have same length as x & y\n!")
       x <- x[number > 0]
       y <- y[number > 0]
       number <- number[number > 0]
    }
    n <- length(x)
    if(!add) {
       axislabels <- match(c("xlab", "ylab"), names(list(...)))
       if(!is.na(axislabels[1]))
          xlab <- list(...)[[axislabels[1]]]
       else xlab <- deparse(substitute(x))
       if(!is.na(axislabels[2]))
          ylab <- list(...)[[axislabels[2]]]
       else ylab <- deparse(substitute(y))
       plot(x, y, xlab = xlab, ylab = ylab, type = "n", ...)
    }
    nequ1 <- number == 1
    if(any(nequ1))
       points(x[nequ1], y[nequ1], pch = pch, csi = size * 1.25)
    if(any(!nequ1))
       points(x[!nequ1], y[!nequ1], pch = pch, csi = size * 0.80000000000000004
          )
    i.multi <- (1:n)[number > 1]
    if(length(i.multi)) {
       ppin <- par()$pin
       pusr <- par()$usr
       xr <- (size * abs(pusr[2] - pusr[1]))/ppin[1]
       yr <- (size * abs(pusr[4] - pusr[3]))/ppin[2]
       i.rep <- rep(i.multi, number[number > 1])
       z <- NULL
       for(i in i.multi)
          z <- c(z, 1:number[i] + if(rotate) runif(1) else 0)
       deg <- (2 * pi * z)/number[i.rep]
       segments(x[i.rep], y[i.rep], x[i.rep] + xr * sin(deg), y[i.rep] + yr * 
          cos(deg))
    }
}
, comment = "14/02/1997")
