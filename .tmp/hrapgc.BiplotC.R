BiplotC <-
structure(function(xx, comps = c(1, 2), cex = c(.8, .4), pchs = 0:6, 
           colour.c = c("black"), colour.v = c("grey"), scale = 1,
           clust = 1, no.clust = 3, lab.cex = 1.2, ax.cex  = 0.9,
           xylab = "PC", expand = 1, xlim = NULL, ylim = NULL, 
           main = NULL, xlab = NULL, ylab = NULL)
{
  ## Purpose: Adds more options to BiplotB
  ## ----------------------------------------------------------------------
  ## Modified from: BiplotB
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##     colour.c:  colour vector for cases
  ##     colour.v:  colour vector for variables
  ##     clust:     are we clustering cases (1) or variables (2)?
  ##                If no clustering is wanted, use BiplotB2
  ##     no.clust   number of clusters
  ##     xylab:     Prefix to x- and y-labels
  ## ----------------------------------------------------------------------
  ## Author:  Zaphod Beeblebrox, Date: 16 Aug 2006, 09:49 

  if(class(xx) == "princomp"){
    scores <- xx$scores[, paste("Comp", comps, sep = ".")]
    loadings <- xx$loadings[, paste("Comp", comps, sep = ".")]
  }
  if(class(xx) == "prcomp"){
    scores <- xx$x[, paste("PC", comps, sep = "")]
    loadings <- xx$rotation[, paste("PC", comps, sep = "")]
  }
  importance <- round(100*xx$sdev/max(cumsum(xx$sdev)), 1) # Variation attributed
  import.lab <- importance[comps]
  x <- scores
  y <- loadings
  n <- nrow(scores)
  p <- nrow(loadings)
## Copy in lam stuff from the default (omit n.obs stuff)
  lam <- xx$sdev[comps]
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) 
    warning("'scale' is outside [0, 1]")
  if (scale != 0) 
    lam <- lam^scale
  else lam <- 1
  x <- t(t(scores)/lam) 
  y <- t(t(loadings)*lam)
  unsigned.range <- function(x) c(-abs(min(x)), abs(max(x)))
  rangx1 <- unsigned.range(x[, 1])
  rangx2 <- unsigned.range(x[, 2])
  rangy1 <- unsigned.range(y[, 1])
  rangy2 <- unsigned.range(y[, 2])
### fix up xlim and ylim
  if (missing(xlim) && missing(ylim)) 
    xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
  else if (missing(xlim)) 
    xlim <- rangx1
  else if (missing(ylim)) 
    ylim <- rangx2
### Ratio of variable sclae to cases scale
  ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
  blank.plot(range(x), range(x), xlim = xlim, ylim = ylim)
  ## work out scaling factor for the loadings
  xax.tik <- pretty(range(xlim))
  xax.tik.at <- xax.tik 
  yax.tik <- pretty(range(ylim))
  yax.tik.at <- yax.tik 
  box()
  abline(v = 0, col = "grey80")
  abline(h = 0, col = "grey80")
### Decide on what clustering is to happen
  if(clust == 1 | clust == "case"){ # We're clustering cases
    if(length(colour.c) < no.clust)
        colour.pal.c <- brewer.pal(8, "Dark2")[1:no.clust]
    else colour.pal.c <- colour.c
    if(length(colour.c) < nrow(x)){# cluster colours aren't known yet
      case.cluster <- pam(x, no.clust)
      colour.c <- colour.pal.c[case.cluster$clustering]
    }
  }
  if(clust == 2 | clust == "variable"){ # We're clustering variables
     if(length(colour.v) < no.clust)
       colour.pal.v <- brewer.pal(8, "Dark2")[1:no.clust]
     else colour.pal.v <- colour.v
     variable.cluster <- pam(y, no.clust)
     colour.v <- colour.pal.v[variable.cluster$clustering]
   }
 
  items <- rownames(scores) # can stay non-generic for now
  ## Get same letters into groups
  ## regions <- gsub("[0-9]", "", items)
  if(!is.null(names(pchs)))# mark cases with point characters
    points(x[names(pchs), choose], pch = pchs, cex = 3, col = "thistle")
  text(x[,1], x[,2], items, cex = cex[1], col = colour.c)
  axis(2, cex.axis = ax.cex, mgp = c(1, .8, 0), at = yax.tik.at,
       labels = yax.tik)
  axis(1, cex.axis = ax.cex, mgp = c(1, .8, 0), at = xax.tik.at,
       labels = xax.tik)
  if(is.null(xlab))
    xlab <- ppaste(xylab, comps[1], " (",import.lab[1], "%)")
  if(is.null(ylab))
    ylab <- ppaste(xylab, comps[2], " (",import.lab[2], "%)")
  mtext(xlab, side = 1, line = 2.2, cex = lab.cex)
  mtext(ylab, side = 2, line = 2.5, cex = lab.cex)

    text(y[, 1]/ratio, y[, 2]/ratio,
         rownames(loadings), cex = cex[2], col = colour.v)
}
, comment = "30/11/2006")
