likelihood.ci <-
structure(function(cm.n = c(0, 0), heatval, xbar, dead, tot, offset = 0, link = "cloglog",
    xfun.inv = exp, interval = T, pc = c(50, 99), dp = 1, save.resid = F, 
    cm.strategy = "adjust.later", full.robust = T)
{
# Uses robust glm
# heatval is the x-variable.
# xbar is a (usually centred) origin of measurement for heatval
  cm <- cm.n[1]
    numcm <- cm.n[2]
    pfrac <- pc/100
    if(cm.strategy == "adjust.later")
       pval <- cm + (1 - cm) * pfrac
    else pval <- pfrac
    n <- length(heatval)
    xvec <- heatval
    link.fun <- link.function(link)    # inv.fun <- link.inverse(link)
    resid <- NULL
    resid.dev <- NULL
    if(cm.strategy == "adjust.later") {
       p <- dead/tot    #  ymat <- cbind(dead, tot - dead)
       reflect.logsurv <- 1
       if(link == "logsurv") {
          ymat <- cbind(tot - dead, dead)
          reflect.logsurv <- -1
       }
       if(link == "log" | link == "logsurv")
          fam <- quasi(link = log, variance = "mu(1-mu)")
       else fam <- call("quasibinomial", link)
       pnx <- data.frame(p.obs = p, x = heatval)
       u.call <- call("glm", p.obs ~ x, data = pnx, family = fam, weight = tot)
       u <- eval(u.call)
       if(full.robust) {
          r1 <- residuals(u, type = "deviance")
          here.r <- abs(r1) != max(abs(r1))
          u2.call <- call("glm", p.obs ~ x, family = fam, data = pnx[here.r,  ],
             weight = tot[here.r])
          u2 <- eval(u2.call)
          assign("startr", predict(u2, newdata = pnx), frame = 1)
          u3.call <- call("glm", p.obs ~ x, family = fam, data = pnx, start = 
             startr, weight = tot)
          u <- eval(u3.call)
       }
       if(any(abs(u$weights) == Inf)) {
          u$weights[abs(u$weights) == Inf] <- 0    
    # Fudge to ensure calculations continue
       }
       uu <- summary.glm(u, correl = F)
       if(save.resid) {
          resid.dev <- residuals(u, type = "deviance") * reflect.logsurv
          resid <- residuals(u, type = "response") * reflect.logsurv
       }
       b0 <- uu$coef[1, 1] * reflect.logsurv
       b1 <- uu$coef[2, 1] * reflect.logsurv
       cm.est <- NULL    # phat <- inv.fun(b0 + b1 * xvec)
       fit.p <- fitted(u)
       if(link == "logsurv")
          fit.p <- 1 - fit.p
       rob.info <- robust.deviance(fit.p, tot, dead)
       dev.robust <- rob.info[[1]]
       df.robust <- rob.info[[2]]
       dev0 <- uu$deviance
       disp0 <- uu$dispersion
       df0 <- uu$df[2]
    }
    else if(cm.strategy == "abbott") {
       uu <- abbott(cmobs = cm, numcm = numcm, dose = heatval, dead = dead, 
          total = tot, link = link, max.iter = 12, save.resid = save.resid)
       b0 <- uu$coef[1]
       b1 <- uu$coef[2]
       cm.est <- uu$cm
       dev0 <- uu$dev
       df0 <- length(heatval) - 2
       if(df0 > 0)
          disp0 <- dev0/df0
       else disp0 <- NA
       if(save.resid) {
          resid.dev <- uu$resid.dev
          resid <- uu$resid
       }
       dev.robust <- NULL
       df.robust <- NULL
    }
    else stop(paste("Unrecognized cm strategy:", cm.strategy))
    cat("Dev. =", format(round(dev0, 2)), "(df", df0, ")", "  [Disp =", format(
       round(disp0, 3)), "]", "  [Dev =", format(round(dev.robust, 2)), "(df", 
       df.robust, ")]", fill = T)
    if(!is.na(disp0)) {
       if(disp0 < 1)
          het <- 1
       else het <- disp0
    }
    else het <- NA
    if(!is.na(disp0)) {
       if(disp0 > 1)
          df.t <- df0
       else df.t <- Inf
    }
    else df.t <- NA
    v11 <- (uu$cov.un[1, 1]) * het
    v22 <- (uu$cov.un[2, 2]) * het
    v12 <- (uu$cov.un[1, 2]) * het
    ld <- xfun.inv((link.fun(pval) - b0)/b1 + xbar) - offset
    dp <- 2 - floor(log(ld[length(ld)])/log(10))
    if(is.na(dp))
       dp <- 1
    dp <- max(dp, 0)
    selog <- array(, length(pc))
    j <- 0
    if(!is.na(v22))
       for(percent in pval) {
          j <- j + 1
         if(b1/sqrt(v22) > 0.5) {
            ci <- fieller(percent, b0, b1, v11, v12, v22, df.t = df.t, link = 
                           link)
           if(is.null(ci$var))
             ci$var <- NA
             ld[j] <- xfun.inv(ci$xhat + xbar) - offset
             selog[j] <- sqrt(ci$var)
             cat(paste("LT", pc[j], ":", sep = ""), format(round(ld[j], dp)),
                "  ", fill = F)
             if(interval)
                if(ci$upper > ci$lower) {
                   cat("95% C.I. is", format(round(xfun.inv(ci$lower + xbar) - 
                      offset, dp)), " to", format(round(xfun.inv(ci$upper + 
                      xbar) - offset, dp)), fill = T)
                }
                else cat("  * CI could not be calculated (g =", paste(round(ci$
                      g, 5), ") *", sep = ""), fill = T)
          }
          else cat(paste("LT", pc[j], ":",  sep = ""), format(round(ld[j], dp
                )), "   ", fill = T)
       }
    else {
       for(i in seq(along = pc))
          cat(paste("   LT", pc[i], ":", sep = ""), format(round(ld[i], dp)))
       cat("\n")
    }
    cept <- b0 - b1 * xbar
    se0 <- sqrt(v11)
    se1 <- sqrt(v22)
    cept <- b0 - b1 * xbar
    var.cept <- v11 + v22 * xbar^2 - 2 * xbar * v12
    rho.cept.b <- (v12 - xbar * v22)/sqrt(var.cept)/se1
    se <- c(sqrt(var.cept), se1)
    rho <- v12/(se0 * se1)
    cat("   Coeffs:", format(round(c(cept, b1), 4)), "[SE", format(round(se, 3)
       ), " r=", format(round(rho.cept.b, 5)), "]", "\n")
    list(coef = c(b0, b1), selog = selog, vcov = c(v11, v12, v22), df = df0, cm
        = cm.est, dev = dev0, dev.robust = dev.robust, df.robust = df.robust, 
       ld = ld, resid = resid, resid.dev = resid.dev)
}
, comment = "14/12/2004")
