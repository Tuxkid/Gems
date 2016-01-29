fitconf <-
structure(function(link = "cloglog", mins, dead, tot, cutoff = NULL, offset, j, leg, 
           interval = F, pc = c(line = 50, line = 99), pc.spline = NULL, pc.monotone
           = c(line = 50, line = 99), span.mono = 1, plot.spline = F, plot.monotone
           = NULL, cutoff.mono = 0, xfun = function(x)
           x, cm = NULL, numcm = NULL, p.min = NULL, printdat = F, save.resid = F, 
           cm.strategy = "adjust.later", cm.code = 0, cm.allcodes = NULL, xfun.inv = 
           NULL, full.robust = F)
{
# link is link function for binomial model
# mins is time of sampling
# dead is number of dead
# total is total number
# cutoff - data at less than this time are not included in the fitting
# offset is the amount added to time to improve linearity
# j is the dataset index number
# log time is used in calculations
# calculations are done using mean zero data
#
# nmid is the number of points suitable for fitting
  if(is.null(xfun.inv)) xfun.inv <- function(x)
    x
  if(all(xfun(exp(1:5)) == (1:5))) {
    takelog <- T
    xfun.inv <- exp
  }
  else takelog <- F
#browser()
  if(all(xfun(1:8) == (1:8)))
    xfun.inv <- function(x)
      x
  if(all(xfun(1:8) == (1:8)^2)) xfun.inv <- sqrt    
# link choices are "identity", "log", "logsurv", "logit", "sqrt", 
#               "inverse", "probit", "cloglog", "loglog"
  inv.fun <- link.inverse(link)    # get inverse of link function
  link.fun <- link.function(link)    # get link function
  if(any(dead > tot))
    stop(paste("\none or more totals is less than no of dead\n", "Dead:", 
               paste(dead, collapse = " "), "\nTotal:", paste(tot, collapse = " ")))
  mins <- as.character(mins)
  cmrows <- mins == cm.code[1]
  if(sum(cmrows) == 0 & length(cm.code) > 1) {
    cmrows <- mins == cm.code[2]
    second.cm <- T
  }
  else second.cm <- F
  x.all <- array(, length(mins))
  x.all <- array(, length(mins))
  x.all[cmrows] <-  - Inf
  cutit <- !cmrows & tot > 0
  if(!is.null(cm.allcodes)) {
    other.rows <- as.logical(match(mins, cm.allcodes, nomatch = 0))
    if(any(other.rows))
      cutit[other.rows] <- F
    x.all[!cutit & !cmrows] <- NA
    x.cm <- mins[other.rows]
    dead.cm <- dead[other.rows]
    tot.cm <- tot[other.rows]
  }
  else x.cm <- NULL
  interp.rows <- cutit | mins == "0"
  if(is.null(cutoff.mono))
    cutoff.mono <- min(c(0, x.all[cutit]))
  mono.rows <- cutit | mins == "0"
  x.all[mono.rows] <- as.numeric(mins[mono.rows])
  if(any(is.na(x.all[cutit])))
    stop(paste("Illegal code", mins[cutit][is.na(x.all[cutit])], 
               "has appeared."))
  mono.rows[mono.rows][x.all[mono.rows] < cutoff.mono] <- F
  if(!is.null(pc.monotone) | !is.null(plot.monotone))
    monospeak <- paste("Monotone fit: Include x >= ", cutoff.mono, ":", sep
                       = "")
  else monospeak <- ""
  if(!is.null(cutoff)) {
    cutit[cutit] <- x.all[cutit] >= cutoff
    cutspeak <- paste("Line: Include x >= ", cutoff, ":", sep = "")
  }
  else cutspeak <- ""
  if(!is.null(p.min)) {
    p <- dead/tot
    omitspeak <- paste("Omit until p > ", format(round(p.min, 2)), ":", sep
                       = "")
    testseq <- seq(along = x.all)[p < p.min]
    if(length(testseq) > 0)
      omit <- 1:max(testseq)
    else omit <- 0
  }
  else {
    omit <- 0
    omitspeak <- ""
  }
  cat("\n", j, "  ", leg, fill = T)
  cutit[omit] <- F
  if(is.null(numcm)) numcm <- 0    # numcm may be reset below
  if(!is.null(cm)) {
    if(numcm == 0)
      cmspeak <- paste("   cm (taken as fixed) =")
    else cmspeak <- "  cm (supplied) ="
    cmspeak <- paste(cmspeak, format(round(cm, 3)))
  }
  else {
    if(sum(cmrows) > 0) {
      if(any(x.all[cmrows] !=  - Inf & x.all[cmrows] != 0))
        cat("\n*** Warning: Fault in specification of cm rows ***", "\n")
      dead0 <- sum(dead[cmrows])
      tot0 <- sum(tot[cmrows])
      cm <- dead0/tot0
      numcm <- tot0
      n.cm <- sum(cmrows)
      cmspeak <- paste("   cm(obs) =", format(round(cm, 3)), "(from", n.cm,
                       paste("point", switch((n.cm > 1) + 1,
                                             "",
                                             "s",
                                             ), ")", sep = ""))
      if(second.cm)
        cmspeak <- paste(cmspeak, "  cm code (2nd choice) =", cm.code[2])
    }
    else cmspeak <- "No information is available on cm"
  }
  if(!is.null(pc.spline)) {
    pfrac.interp <- pc.spline/100
    yhat.pred <- link.fun(pfrac.interp)
    dead.interp <- dead[interp.rows]
    tot.interp <- tot[interp.rows]
    p.obs <- (dead.interp/tot.interp)
    time.interp <- as.numeric(mins[interp.rows])
    time1 <- max(time.interp[p.obs == 0])
    if(is.na(time1))
      time1 <- min(time.interp)
    time2 <- min(time.interp[p.obs == 1])
    if(is.na(time2))
      time2 <- max(time.interp)
    time.here <- (time.interp >= time1) & (time.interp <= time2)
    dead.interp <- dead.interp[time.here]
    tot.interp <- tot.interp[time.here]
    p.interp <- (dead.interp + 1/6)/(tot.interp + 1/3)
    x.interp <- time.interp[time.here]
  }
  ##
  if(!is.null(pc.monotone)) {
    time.here <- as.numeric(mins[mono.rows])
    ord.x <- order(time.here)
    subs.here <- (1:length(mono.rows))[mono.rows][ord.x]
    dead.mono <- dead[subs.here]
    tot.mono <- tot[subs.here]
    p.obs <- (dead.mono/tot.mono)
    time.mono <- time.here[ord.x]
  }
  ##
  ld.dp <- 2 - floor(log(max(x.all[cutit], na.rm = T))/log(10))
  ld.dp <- max(0, ld.dp)
  if(is.na(ld.dp))
    ld.dp <- 1
  if(nchar(monospeak) > 0)
    cat(monospeak, "\n")
  if(nchar(cutspeak) + nchar(omitspeak) > 0)
    cat(cutspeak, " ", omitspeak)
  cat(sum(cutit), "points remain_")
  cat(cmspeak, "\n")
  if(takelog) {
    x.trt <- log(x.all[cutit] + offset)
  }
  else {
    x.trt <- xfun(x.all[cutit] + offset)
  }
  xbar <- mean(x.trt)
  x.explan <- x.trt - xbar
  dead2 <- dead[cutit]
  tot2 <- tot[cutit]
  nmid <- length(x.trt[dead2 < tot2])
  lt <- rep(-999, length(pc))
  b0 <- -999
  b1 <- -999
  selog50 <- -999
  selog99 <- -999
  cept <- NA
  dev <- NA
  df <- NA
  dev.robust <- NA
  df.robust <- NA
  disp <- NA
  resid <- NULL
  resid.dev <- NULL
  lt.se <- rep(NA, length(pc))
  cm.est <- cm
  cm.n <- c(cm, numcm)
  if(!is.null(pc.monotone) && (length(time.mono) >= 2)) {
    if(link == "loglog")
      link <- "cloglog"
    lt.monotone <- pool.adj(time.mono, dead.mono, tot.mono, phat = pc.monotone,
                            cm = cm, cm.strategy = cm.strategy, cm.allcodes = 
                            cm.allcodes, plotit = plot.monotone, link = link,
                            xfun = xfun, legend = leg, x.cm = x.cm,
                            dead.cm = dead.cm, tot.cm = tot.cm, span = 
                            span.mono)
    note <- c("( loess)", "( monotone line)")[names(lt.monotone)]
    cat(paste("   LT", pc.monotone, ":", format(round(lt.monotone, ld.dp)), 
              sep = ""), note, "\n")
  }
  else lt.monotone <- NULL
  if(!is.null(pc.spline)) {
    lt.spline <- interp.curve(x.interp, p = p.interp, pval = pfrac.interp, 
                              plot.interp = plot.spline, fun = link.fun)
    cat(paste("   LT", pc.spline, ":",  , sep = ""),
        format(round(lt.spline, ld.dp)), "(spline)   ", "\n")
  }
  else lt.spline <- NULL
  if(nmid == 2)
    full.robust <- F
###  browser()
  if(nmid >= 2) {
    u <- likelihood.ci(cm.n = cm.n, x.explan, xbar, dead2, tot2, link = link,
                       xfun.inv = xfun.inv, interval = interval, pc = pc,
                       save.resid = save.resid, cm.strategy = cm.strategy,
                       full.robust = full.robust)
    lt <- u$ld
    ld.dp <- 2 - floor(log(lt[length(lt)])/log(10))
    if(is.na(ld.dp))
      ld.dp <- 1
    ld.dp <- max(ld.dp, 0)
    b0 <- u$coef[1]
    b1 <- u$coef[2]
    cept <- b0 - b1 * xbar
    lt.se <- u$selog
    resid <- u$resid
    resid.dev <- u$resid.dev
    dev <- u$dev
    df <- u$df
    dev.robust <- u$dev.robust
    df.robust <- u$df.robust
    disp <- u$dispersion
    linpred <- b0 + b1 * x.explan    #  comprob <- inv.fun(linpred)
###  fitr <- (1 - comprob) * tot2
###  fitc <- comprob * tot2
    if(printdat) print(cbind(x.all[cutit], dead2, tot2))    
###   print(cbind(x.all[cutit], x.explan, fitr, dead2, tot2))
###  if(min(fitr) < 1) {
###   cat("Warning.  Some expected numbers 0f dead are < 1", 
###    fill = T)
###   cat("Exp. No. Dead:", format(round(fitr, 3)), fill = T)
###   
###  }
###  if(min(fitc) < 1) {
###   cat(
###    "Warning.  Some expected numbers of survivors are < 1", 
###    fill = T)
###   cat("Exp. Survivors   :", format(round(fitc, 3)), fill
###     = T)
###  }
  }
  else {
### Estimates of LT50 and LT90 when too few data points are available
                                        ###
    if(printdat) print(cbind(x.all, dead, tot, cutit))
    cat(nmid, " data points give mortalities < 1.0:     ")
    lt <- NA
    cat("No estimate is available", "\n")
  }
  if(!save.resid) tot2 <- NULL    
### Keep totals only if required along with residual information 
  list(lt = lt, se = lt.se, b0 = cept, b1 = b1, lt.spline = lt.spline, 
       lt.monotone = lt.monotone, cm = cm.est, dev = dev, df = df, dev.robust
       = dev.robust, df.robust = df.robust, dispersion = disp, total = tot2, 
       resid = resid, resid.dev = resid.dev, times = x.trt)
}
, comment = "14/09/2004")
