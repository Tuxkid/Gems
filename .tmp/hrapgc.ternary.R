ternary <-
structure(function(X, pch = par("pch"), lcex = 1, add = FALSE,
                    ord = 1:3, space = 1, ...)
{
### Purpose: Draws ternary plots
### ----------------------------------------------------------------------
### Modified from: MASS Skye dataset
### ----------------------------------------------------------------------
### Arguments: space: how much space needed to allow for long apex names?
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date:  7 Apr 2004, 10:01
  X <- as.matrix(X)
  if(any(X) < 0) stop("X must be non-negative")
  s <- drop(X %*% rep(1, ncol(X)))
  if(any(s <= 0)) stop("each row of X must have a positive sum")
  if(max(abs(s - 1)) > 1e-6) {
    warning("row(s) of X will be rescaled")
    X <- X / s
  }
  X <- X[, ord]
  s3 <- sqrt(1/3)
  if(!add)
    {
      oldpty <- par("pty")
      on.exit(par(pty = oldpty))
      par(pty = "s")
      plot(c(-s3, s3) * space, c(0.5 - s3, 0.5 + s3) * space, type = "n",
           axes = FALSE, xlab = "", ylab = "")
      polygon(c(0, -s3, s3), c(1, 0, 0), density = 0)
      lab <- NULL
      if(!is.null(dn <- dimnames(X))) lab <- dn[[2]]
      if(length(lab) < 3) lab <- as.character(1:3)
      eps <- 0.05 * lcex
      text(c(0, s3 + eps * 0.7, -s3 - eps * 0.7),
           c(1 + eps, -0.1 * eps, - 0.1 * eps), lab, cex = lcex)
    }
  points((X[, 2] - X[, 3]) * s3, X[, 1], pch = pch, ...)
}
, comment = "15/04/2004")
