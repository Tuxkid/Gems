pool.adj <-
structure(function(x, r, n, phat, cm, cm.strategy = "adjust.later", cm.code = NULL, 
           cm.allcodes = NULL, plimits = c(0.0025, 0.9995), plotit = "loess",
         link = "loglog", xfun = function(x) x, legend = "", x.cm = NULL,
         dead.cm = NULL, tot.cm = NULL, xtit = "Time", ytit = NULL,
         mkh = 0.025, extrap.low = 0.25, extrap.high = 0.25, span = 1,
         deg = 1, lo.lty = 2, lwd = 1, n.lo = 50, title.line = 0.25)
{
### pool.adj is called both by fitconf and by flyplot
###
### extrap.low (extrap.high) is fraction
### of range allowed for extrapolation below (above)
### These are halved if there are just two points
  if(!is.null(plotit)) {
    plotit[plotit == "loess"] <- "mono.loess"
    plotit[plotit == "mono"] <- "mono.loess"
    show.curve <- as.logical(match(c("mono.line", "mono.loess"), plotit, 
                                   nomatch = 0))
  }
  else show.curve <- rep(F, 2)
  if(!is.null(phat)) {
    phat.strategy <- names(phat)
    new.lt <- T
    lt <- array(NA, length(phat))
    nam.lt <- names(phat)
    ab <- vector("list", length(phat))
  }
  else {
    lt <- NULL
    ab <- NULL
    new.lt <- F
    phat.strategy <- ""
  }
  plot.some <- any(show.curve)
  if(all(xfun(exp(1:8)) == 1:8)) {
    xfun.inv <- exp
    takelog <- T
  }
  else {
    xfun.inv <- function(x)
      x
    takelog <- F
  }
  if(any(diff(x)) < 0)
    stop("** Monotone fitting requires that data is ordered by x-values **")
  x0 <- x
  r0 <- r
  n0 <- n
  p0 <- r0/n0
  if(any(diff(x) == 0)) {
    r <- tapply(r, x, sum)
    n <- tapply(n, x, sum)
    x <- as.numeric(names(r))
  }
  if(takelog)
    if(any(x <= 0)) {
      x.0 <- x > 0
      x <- x[x.0]
      r <- r[x.0]
      n <- n[x.0]
    }
  g <- link.function(link)
  nn <- length(n)
  p <- r/n
  if(is.null(cm)) {
    cat("\nNo control mortality was supplied.\n")
    cat("\nFor monotone estimation set cm = 0\n")
  }
  if(cm.strategy == "adjust.later" & !is.null(phat))
    ptarg <- cm + ((1 - cm) * phat)/100
  id <- 1:nn
  repeat {
    i <- 2
    while(i <= nn && p[id[i]] >= p[id[i - 1]]) i <- i + 1
    if(i <= nn) {
      x[id[i - 1]] <- x[id[i]] <- (n[id[i - 1]] * x[id[i - 1]] + n[id[i]] * 
                                   x[id[i]])/(n[id[i - 1]] + n[id[i]])
      r[id[i - 1]] <- r[id[i]] <- r[id[i]] + r[id[i - 1]]
      n[id[i - 1]] <- n[id[i]] <- n[id[i]] + n[id[i - 1]]
      p[id[i - 1]] <- p[id[i]] <- r[id[i]]/n[id[i]]
      idi <- id[i]
      while(i <= nn && id[i] == idi) {
        id[i] <- id[i - 1]
        i <- i + 1
      }
    }
    else break
  }
  n1 <- max((1:nn)[p == 0])
  if(!is.na(n1))
    id <- id[id >= n1]
  use <- unique(id)
  ux <- x[use]
  ux.tran <- xfun(ux)
  if(!is.null(phat))
    ptarg.interp <- ptarg
  r.use <- r[use]
  n.use <- n[use]
  p.use <- r.use/n.use
  if(cm.strategy == "abbott") {
    here.use <- p.use >= cm
    use <- use[here.use]
    ux <- ux[here.use]
    ux.tran <- xfun(ux)
    p.use <- (p.use[here.use] - cm)/(1 - cm)
  }
  xx <- cbind(r[use], (n - r)[use])
  p.max <- max(p.use)
  p.min <- min(p.use)
  done.loess <- F
  show.line <- show.curve[1]
  show.loess <- show.curve[2]
  nx <- sum(p.use < 1)
  n01 <- sum(p.use > 0 & p.use < 1)
  phat.strategy[phat.strategy == "mono"] <- "loess"
  phat.strategy[phat.strategy == "mono.loess"] <- "loess"
  calc.loess <- any(as.logical(match("loess", phat.strategy, nomatch = 0)))
  if(length(ux) >= 4 & nx >= 3 & (show.loess | calc.loess) & n01 >= 2) {
    gp <- g(p.use)
    df.lo <- data.frame(y = p.use, x = ux.tran, n = n.use)
    browser()
    u <- switch(link,
                identity = gam(y ~ lo(x, span = span, degree = deg), family = 
                  binomial("identity"), data = df.lo, weight = n), logsurv = ,
                log = gam(y ~ lo(x), family = binomial("log"), data = df.lo,
                  weight = n, span = span, degree = deg),
                logit = gam(y ~ lo(x, span = span, degree = deg),
                  family = binomial("logit"), data = df.lo, weight = n),
                sqrt = gam(y ~ lo(x, span = span, degree = deg),
                  family = binomial("sqrt"), data = df.lo, weight = n),
                inverse = gam(y ~ lo(x, span = span, degree = deg),
                  family = binomial("inverse"), data = df.lo, weight = n),
                probit = gam(y ~ lo(x, span = span, degree = deg),
                  family = binomial("probit"), data = df.lo, weight = n),
                cloglog = ,
                loglog = gam(y ~ lo(x, span = span, degree = deg),
                  family = binomial("cloglog"), data = df.lo, weight = n))
    df.new <- data.frame(x = pretty(ux.tran, n.lo))
    hat.u <- predict(u)
    u.lo <- loess(y ~ x, data = list(y = hat.u, x = ux.tran), span = span, 
                  deg = deg, span = span)    # If no. of points is 4 and span<1, 
### the fitted curve has a tendency to oscillate up and down
    hat.loess <- predict(u.lo, newdata = df.new)
    range.loess <- range(hat.loess, na.rm = T)
    done.loess <- T
  }
  else done.loess <- F
  for(i in seq(along = phat)) {
    if(n01 < 2)
      break
    pc <- ptarg[i]
    yhat <- g(pc)
    get.lt <- as.logical(match(c("line", "loess"), phat.strategy[i], nomatch
                               = 0))
    lt.line <- get.lt[1]
    lt.loess <- get.lt[2]
    pc.interp <- ptarg.interp[i]
    yhat.interp <- g(pc.interp)
    alt.line <- F
    if(!done.loess || yhat.interp < range.loess[1] || yhat.interp > 
       range.loess[2])
      alt.line <- T
    mn.use <- 1:length(use)
    if(any(r.use < n.use))
      n.ones <- max(mn.use[r.use < n.use]) + 1
    else n.ones <- mn.use[1]
    take.above <- (r.use > n.use * pc) & (mn.use <= n.ones)
    take.low <- r.use <= n.use * pc
    if(lt.line | (alt.line & lt.loess))
      if(sum(take.above) >= 1) {
        mn.above <- mn.use[take.above]
        n.above <- min(c(max(mn.above), min(mn.above) + 2))
        n.low <- max(c(1, n.above - 3))
        if(n.low == 1)
          n.above <- min(c(max(mn.above), min(mn.above) + 3))
      }
      else if(sum(take.low) >= 1) {
        mn.low <- mn.use[take.low]
        n.above <- max(mn.low)
        n.low <- max(c(mn.low[1], n.above - 3))
      }
      else {
        lt.line <- F
        alt.line <- F
      }
    if(lt.line | alt.line) {
      p.here <- p.use[n.low:n.above]
      not.one <- sum(p.here < 1)
      g.here <- g(p.here)
      g.interp <- g(pc.interp)
      n.span <- n.above - n.low + 1
      eps.low <- extrap.low * diff(range(g.here))
      eps.high <- extrap.high * diff(range(g.here))
      if(n.span == 2) {
        eps.low <- eps.low/2
        eps.high <- eps.high/2
      }
      if(n.span >= 2 & not.one > 1)
        show.line <- ((max(g.here) > pc.interp - eps.high & min(g.here) < 
                       pc.interp) | (min(g.here) < pc.interp + eps.low &
                                     max(g.here) > pc.interp))
      else {
        show.line <- F
        lt.line <- F
        alt.line <- F
      }
    }
    if(lt.line | (alt.line & lt.loess)) {
      rn <- xx[n.low:n.above,  ]
      xv <- xfun(ux[n.low:n.above])
      u <- switch(link,
                  identity = glm(y ~ x, family = binomial("identity"),
                    data = list(y = rn, x = xv)), logsurv = ,
                  log = glm(y ~ x, family = binomial("log"),
                    data = list(y = rn, x = xv)),
                  logit = glm(y ~ x, family = binomial("logit"),
                    data = list(y = rn, x = xv)),
                  sqrt = glm(y ~ x, family = binomial("sqrt"),
                    data = list(y = rn, x = xv)),
                  inverse = glm(y ~ x, family = binomial("inverse"),
                    data = list(y = rn, x = xv)),
                  probit = glm(y ~ x, family = binomial("probit"),
                    data = list(y = rn, x = xv)),
                  cloglog = ,
                  loglog = glm(y ~ x, family = binomial("cloglog"),
                    data = list(y = rn, x = xv)))
      if(!is.na(u$coef[2]) & (lt.line | (alt.line & lt.loess)) & u$coef[2] >
         0) {
        lt[i] <- xfun.inv((yhat - u$coef[1])/u$coef[2])
        ab[[i]] <- u$coef[1:2]
        nam.lt[i] <- "line"
      }
      else {
        lt[i] <- NA
        ab <- NULL
      }
    }
    if(done.loess && lt.loess && min(hat.loess, na.rm = T) <= yhat && max(
                                      hat.loess, na.rm = T) >= yhat) {
      here.hat <- !is.na(hat.loess)
      lt[i] <- xfun.inv(approx(x = hat.loess[here.hat],
                               y = df.new$x[ here.hat], xout = yhat.interp)$y)
      nam.lt[i] <- "mono"
    }
  }
  if(new.lt & !is.null(phat)) {
    p.txt <- paste("LT", phat, "=", format(round(lt, 1)), sep = "")
    leg <- paste(legend, " [", paste(p.txt, collapse = "; "), "]", sep = "")
    names(lt) <- nam.lt
  }
  else leg <- legend
  if(plot.some) {
    if(!is.null(x.cm)) {
      x0 <- c(paste(x0), x.cm)
      r0 <- c(r0, dead.cm)
      n0 <- c(n0, tot.cm)
    }
### NB x0, r0, n0 do not appear below
    simplot(x = x0, resp = r0, tot = n0, fun = link, cm.strategy = 
            cm.strategy, cm = cm, cm.code = cm.code,
            cm.allcodes = cm.allcodes, xtit = xtit, ytit = ytit, mkh = mkh,
            xfun = xfun, main = leg, title.line = title.line,
            plimits = plimits, points.lwd = lwd)
    yp <- g(plimits)
    for(j in seq(along = ab))
      abline(ab[[j]], lty = i)
    if(done.loess) {
      here <- hat.loess >= yp[1] & hat.loess <= yp[2]
      lines(df.new$x[here], hat.loess[here], lty = lo.lty, lwd = lwd)
    }
  }
  lt
}
, comment = "01/10/2004")
