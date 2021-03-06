make.line.loess <-
structure(function(xy, lab, label.pos = 0.9, cex = par()$cex, lev.font = 1, lty = 1, lwd
     = par()$lwd, inclusion = "ALL", mark = T, mark.extra = "")
{
    overlap <- function(x, y, xl, yl)
    {
       over <- F
       i <- 1
       while(i <= length(xl)) {
          first.pair.match <- (1:length(xl[[i]]))[(xl[[i]] == x[1]) & yl[[i]] == 
             y[1]]
          if(is.na(diff(x)))
             browser()
          if((diff(x) == 0) & (diff(y) == 0)) {
             if(length(first.pair.match) > 0)
                over <- T
          }
          else {
             second.pair.match <- (1:length(xl[[i]]))[(xl[[i]] == x[2]) & yl[[i
                ]] == y[2]]
             names(first.pair.match) <- rep("f", length(first.pair.match))
             names(second.pair.match) <- rep("s", length(second.pair.match))
             pair.match <- sort(c(first.pair.match, second.pair.match))
             adj.pairs <- (1:(length(pair.match) - 1))[diff(pair.match) == 1]
             adj.pairs <- adj.pairs[!is.na(adj.pairs)]
             if(length(adj.pairs) > 0)
                if((names(pair.match)[adj.pairs[1]] == "f") & (names(pair.match
                   )[adj.pairs[1] + 1] == "s"))
                   over <- T
          }
          i <- i + 1
       }
       over
    }
    assign("overlap", overlap, 0)
    pos <- function(posn, x, y)
    {
       if(posn == 1)
          length(x)
       else {
          dx <- c(0, diff(x))
          dy <- c(0, diff(y))
          lengths <- sqrt(dx^2 + dy^2)
          cum.lengths <- cumsum(lengths)
          full.length <- cum.lengths[length(cum.lengths)]
          main.index <- ((1:length(cum.lengths))[cum.lengths > posn * 
             full.length])[1] - 1
          left.over <- posn * full.length - cum.lengths[main.index]
          main.index + left.over/lengths[main.index + 1]
       }
    }
    assign("pos", pos, 0)
    label.line <- function(label.pos, x, y, lab, cex, i, font)
    {
       if(label.pos >= 0) {
          j <- pos(label.pos, x, y)
          index <- trunc(j)
          if(!is.na(diff(x[index + 0:1])) & !is.na(diff(y[index + 0:1])))
             if(!overlap(x[index + 0:1], y[index + 0:1], x.keep, y.keep)) {
                j <- j - index
                tx <- x[index] + j * (x[index + 1] - x[index])
                ty <- y[index] + j * (y[index + 1] - y[index])
                text(tx, ty, lab, cex = cex, font = font)
                x.keep[[i - 1]] <- x
                y.keep[[i - 1]] <- y
                assign("x.keep", x.keep, 1)
                assign("y.keep", y.keep, 1)
             }
       }
    }
    plot.line <- function(x, y, lty, lwd, inclusion)
    {
       find.point <- function(inclusion, x, y, start)
       {
          p <- pos(inclusion, x, y)
          index <- trunc(p)
          p <- p - index
          tx <- x[index] + p * (x[index + 1] - x[index])
          ty <- y[index] + p * (y[index + 1] - y[index])
          list(tx = tx, ty = ty, index = index + start)
       }
       if(is.character(inclusion)) {
          lines(x, y, lty = lty, lwd = lwd)
          T
       }
       else if(is.null(inclusion))
          F
       else if(inclusion[1] == inclusion[2])
          F
       else {
          if(inclusion[1] > inclusion[2])
             inclusion <- c(inclusion[1], 1, 0, inclusion[2])
          x.save <- x
          y.save <- y
          for(i in 1:(length(inclusion) %/% 2)) {
             x <- x.save
             y <- y.save
             start <- find.point(inclusion[1 + (i - 1) * 2], x, y, T)
             finish <- find.point(inclusion[2 + (i - 1) * 2], x, y, F)
             x <- c(start$tx, x[start$index:finish$index], finish$tx)
             y <- c(start$ty, y[start$index:finish$index], finish$ty)
             lines(x, y, lty = lty, lwd = lwd)
          }
          T
       }
    }
## Main ##
    xy$x <- c(NA, xy$x, NA)
    na <- (1:length(xy$x))[is.na(xy$x)]
    xy$x <- xy$x[ - (na[diff(na) == 1])]
    xy$y <- c(NA, xy$y, NA)
    xy$y <- xy$y[ - (na[diff(na) == 1])]
    na <- (1:length(xy$x))[is.na(xy$x)]
    x.keep <- list()
    y.keep <- list()
    assign("x.keep", x.keep, 1)
    assign("y.keep", y.keep, 1)
    if(length(label.pos) < length(na) - 1)
       label.pos <- c(label.pos, rep(label.pos[1], length(na) - length(
          label.pos)))
    cat("Lev ", lab, mark.extra, " Lines ", max(length(na) - 1, 0), "\n", sep
        = "")
    if(length(na) > 0)
       for(i in 2:length(na)) {
          x <- xy$x[(na[i - 1] + 1):(na[i] - 1)]
          y <- xy$y[(na[i - 1] + 1):(na[i] - 1)]
          old.x <- x
          old.y <- y
          x <- x[!is.na(old.x) & !is.na(old.y)]
          y <- y[!is.na(old.x) & !is.na(old.y)]
          if(length(x) > 0) {
             if(plot.line(x, y, lty, lwd, inclusion[[i - 1]]))
                label.line(label.pos[i - 1], x, y, paste(lab, ifelse(mark, 
                   paste(mark.extra, "-", i - 1, sep = ""), ""), sep = ""), cex,
                   i, font = lev.font)
          }
       }
}
, comment = "03/06/1998")
