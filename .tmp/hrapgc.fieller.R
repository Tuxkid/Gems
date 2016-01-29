fieller <-
structure(function(phat, a, b, v11, v12, v22, df.t = Inf, link = "logit")
{
                                        # fiducial interval calculation from Finney p.78
                                        #
                                        # a,b    estimated intercept and slope
                                        # v11, v12, v22  variance and covariance of estimates of a and b
                                        # t    t statistic
                                        #
  if(df.t == Inf) tt <- 1.96 else tt <- qt(0.975, df.t)
  g.link <- link.function(link)
  a <- g.link(phat) - a
  v12 <- v12* -1
  m <- a/b
  g <- (gg <- (tt/b)^2) * v22

  if(g > 0.99 || g < 0) {
    return(list(xhat = m, ci = NULL, lower = 1, upper = -1, g = g))
  }
  xhat0 <- m + g/(1 - g) * m - gg/(1 - g) * v12
  vv <- v11 - 2 * m * v12 + m^2 * v22 - g * v11 + gg * v12^2
  Ix <- tt/b/(1 - g) * sqrt(vv)
  Ix <- abs(Ix)
  list(xhat = m, var = vv, lower = xhat0 - Ix, upper = xhat0 + Ix, g = g)
}
, comment = "14/09/2004")
