plot.lt <-
structure(function(lt = 99, datafun = get.leaf, choose = "lbam", fun = function(x)
x, poly = T, poly.limits = list(NULL), poly.keep.x = list(NULL), 
    poly.plot.limits = list(NULL), deg = 2, splin = F, wide.legend = 1, 
    high.legend = 1, expand.y = 0, print.statistics = T, x.limits = NULL, 
    y.limits = NULL, predict.x = NULL, confidence = 0.95, compare = NULL, 
    fit.robust = F, plot.ci = F, plotit = T, plot.means = T, plot.points = F, 
    plot.single.sem = F, plot.groups.sem = T, legend.sem = T, legend.linespace
     = 1.25, errors = "aov", include = NULL, parallel = NULL, mkh = 0.08, 
    datestamp = T, prefix = "", prefix.y = "", legend.header = "", legend.note
     = "Limits shown are one SE\neither side of the mean", bars.pretext = 
    "2 SE", na.means = T, wts = NULL, style = list(lwd = 1, plotch = c(0, 1, 2,
    5, 3, 4, 16, 18, 6:15), cex.labels = 1, cex.title = 1.15, mai.xtra = c(0.25,
    0.5, 0.25, 0.5)), mtext.line = -1)
{
### Means, for each temperature in each list item, are calculated and plotted
### An overall SD is calculated for each set of list items, 
### as identified in groups.
### Set na.means to F to omit plotting of means when data for a mean relies
### partly on NAs
### Examples
### plot.lt(lt=50,datafun=get.air.CA.lt,poly=F, poly.limits=list(c(32.5,40)), 
### splin=T,wide.legend=0.67)
### plot.lt(lt=50,datafun=get.co2.o2.lt,poly=c(T,F),deg=1, splin=F,wide.legend=0.67)
### plot.lt(lt=99,datafun=get.co2.o2.lt,poly=c(T,F),deg=2, splin=F,wide.legend=0.67)
### plot.lt(lt=50,datafun=get.pretreat.lt,poly=T,deg=2, splin=F,wide.legend=0.67)
#
### aov.errors = T causes the within x mean square to be used for SEs etc.
#
### lt= : gives lt % to be printed on the y-axis label
#
### datafun: This function collects up the data, labelling information etc., 
### and delivers it in a standard form to plot.lt.
#
### fun: Transformation function for the y-axis.  Usually identity or log.
#
### poly: T if a line or polynomial is required.
#
### poly.limits: used in connection with splin=T when a specific polynomial
### (usually a quadratic) is required over the part of the range specified 
### by poly.limits, with a spline for the remainder.
### 
### deg: degree for polynomial(s)
#
### Note: Parameters poly, poly.limits and deg are expanded, if necessary, 
### into a vector or (for poly.limits) a list.
### wide.legend, high.legend: point at which to start printing first legend item
### Specify as a fraction of the x- or y-distance.
### expand.y: e.g. 25% will increase the upper y-limit to give 25% extra space 
### for legend etc.
#
### print.statistics: whether to print out coeffs, ses etc of fitted polynomials
### plot.means, plot.points: means and/or points
### plot.single.sem, plot.groups.sem: strategy for calculating sem
### errors: strategy for calculation of error mean square.
###  "aov" calculates the mean square from the "within x" sum of squares.
###  Alternatives are lack.of.fit (deviations from the fitted polynomial)
###  and total (aov and lack.of.fit combined).
### include: This is a vector of booleans; T where a list element is included.
#
#
    par(new = F)
    options(width = 180)
    var.new <- function(x)
    {
       pres <- !is.na(x)
       var(x[pres])
    }
    date.txt <- unix("date '+DATE: %d/%m/%y %H:%Mh'")
    docs <- paste(unix("pwd"), as.character(as.name(match.call())), date.txt, 
       collapse = "")
    print(docs)
    if(!plotit) {
       if(missing(plot.means))
          plot.means <- F
       if(missing(plot.points))
          plot.points <- F
    }
    plotch <- style$plotch
    lwd <- style$lwd
    cex.title <- style$cex.title
    cex.labels <- style$cex.labels
    mai.xtra <- style$mai.xtra
    if(!is.null(confidence) & !is.null(compare)) {
       cat("\nValues set both for ci's & for comparisons.", 
          "\nCode for both is not implemented at this point.", 
          "\nWill do comparisons only.\n")
       confidence <- NULL
    }
    if(plot.points | plot.means | plotit)
       plot.some <- T
    else plot.some <- F
    xtra <- numeric(0)
    linetype.len <- 0
    oldpar <- par()
    on.exit(par(oldpar))
    par(mai = par()$mai + mai.xtra)
    u <- datafun(lt = lt, choice = choose)
    leg <- u$leg
    ltraw.list <- u$lt
    xval.list <- u$xval
    xlab <- u$xlab
    xaxlab <- u$xaxlab
    yaxlab <- u$yaxlab
    ylab <- u$ylab
    groups <- u$groups
    maint <- u$maint
    if(is.list(maint))
       maint <- paste(maint[["prefix"]], " LT~v~c.8~.", lt, "~h0~c1~.",  , " ",
          maint[["main"]], sep = "")
    else maint <- paste(prefix, maint)
    mean.list <- lapply(xval.list, unique)    
### Set up structure that has the right shape
    num.list <- mean.list
    uniqx.list <- mean.list
    nlist <- length(xval.list)
    if(is.numeric(include)) {
       tmp <- rep(F, nlist)
       tmp[include] <- T
       include <- tmp
    }
    else if(is.null(include))
       include <- rep(T, nlist)
    else if(length(include) < nlist)
       include <- c(include, rep(F, nlist - length(include)))
    include.parallel <- rep(F, nlist)
    if(!is.null(parallel)) {
       include.parallel[parallel] <- T
       xval.parallel <- list()
       pres.parallel <- list()
       lt.parallel <- list()
       n.parallel <- sum(include.parallel)
       num.parallel <- rep(0, n.parallel)
       wts.parallel <- rep(0, n.parallel)
       df.parallel <- rep(0, n.parallel)
       leg.parallel <- leg[include.parallel]
    }
    if(is.null(groups))
       groups <- as.list((1:nlist)[include])
    ngroups <- length(groups)
    if(nlist > 1 & length(deg) == 1)
       deg.pol <- rep(deg, nlist)
    else deg.pol <- deg
    if(nlist > 1 & length(poly) == 1)
       poly <- rep(poly, nlist)
    if(nlist > 1 & length(splin) == 1)
       splin <- rep(splin, nlist)
    if(!is.list(poly.limits))
       poly.limits <- list(poly.limits)
    if(!is.list(poly.keep.x))
       poly.keep.x <- list(poly.keep.x)
    if(nlist > 1 & length(poly.limits) == 1)
       poly.limits <- rep(poly.limits, nlist)
    if(nlist > 1 & length(poly.keep.x) == 1)
       poly.keep <- rep(poly.keep.x, nlist)
    if(!is.list(poly.plot.limits))
       poly.plot.limits <- list(poly.plot.limits)
    if(nlist > 1 & length(poly.plot.limits) == 1)
       poly.plot.limits <- rep(poly.plot.limits, nlist)
    if(!is.null(wts))
       if(nlist > 1 & length(wts) == 1)
          wts <- rep(wts, nlist)
    xlab <- u$xlab
    if(sum(include) > length(plotch))
       plotch <- c(plotch, letters)
    vm.aov <- array(, nlist)
    vm.other <- array(, nlist)
    df.other <- array(, nlist)
    numdf <- array(, nlist)
    nummin <- array(, nlist)
    nummax <- array(, nlist)
    lt.list <- ltraw.list
    for(i in 1:nlist)
       lt.list[[i]] <- fun(ltraw.list[[i]])
    mat.range <- matrix(unlist(lapply(ltraw.list, range, na.rm = T)), nrow = 2)
    unlist.raw <- unlist(ltraw.list)
    yrange <- range(mat.range[, include], na.rm = T)
    log.y <- F
    if(all(fun(exp(1:4)) == 1:4)) {
       log.y <- T
       mat.range <- matrix(unlist(lapply(ltraw.list, function(x, na.rm = T)
       range(x[x > 0]), na.rm = T)), nrow = 2)
       yrange[1] <- min(mat.range, na.rm = T)
       yrange[1] <- min(unlist.raw[unlist.raw > 0], na.rm = T)
    }
    if(is.null(y.limits)) {
       funrange <- fun(yrange)
       y.limits <- yrange
    }
    else funrange <- fun(y.limits)
    funrange[2] <- funrange[2] + diff(funrange) * expand.y    
### Often set expand.y = 0.05 or 0.1, to allow room for legend
    if(is.null(yaxlab)) {
       ylabels <- pretty(y.limits)
       if(all(fun(exp(1:4)) == 1:4)) {
          splity <- sqrt(y.limits[1] * y.limits[2])
          ylabels2 <- pretty(c(splity, max(y.limits)), 4)
          ylabels1 <- pretty(c(min(y.limits), splity), 4)
          ylabels <- sort(unique(c(ylabels1, ylabels2)))
          if(ylabels[1] == 0)
             ylabels[1] <- mean(ylabels[1:2])
       }
    }
    else ylabels <- yaxlab
    ncharwid <- max(nchar(paste(ylabels)))
    ylabadd <- (ncharwid - 1.5) * 0.25
    ytitladd <- (1.25 * ncharwid - 2) * 0.125
    if(plot.some) {
       if(is.null(x.limits))
          x.limits <- range(unlist(xval.list), na.rm = T)
       plot(x.limits, funrange, xlim = x.limits, type = "n", xlab = "", xaxt = 
          "n", ylab = "", yaxt = "n")
       if(is.null(xaxlab))
          xaxlab <- pretty(x.limits)
       axis(1, at = xaxlab)
       axis(2, at = fun(ylabels), labels = paste(ylabels), mgp = c(3 + ytitladd,
          1 + ylabadd, 0))
       chh <- par()$cxy[2]
       chw <- par()$cxy[1]
       uxy <- par()$usr
    }
    outvals <- vector("list", sum(include))
    names(outvals) <- names(lt.list)[include]
    i.plot <- 0
    j.parallel <- 0
    for(i in 1:nlist)
       if(include[i]) {
          u.x <- NULL
          bt.y <- NULL
          i.plot <- i.plot + 1
          deg.i <- deg.pol[i]
          pres <- !is.na(lt.list[[i]])
          if(all(!pres))
             next
          temps <- xval.list[[i]][pres]
          ltval <- lt.list[[i]][pres]
          if(plot.points)
             points(temps, ltval, pch = plotch[i.plot], mkh = 0.75 * mkh, lwd
                 = lwd)
          vv <- tapply(ltval, list(temps), var)
          my <- tapply(ltval, list(temps), mean)
          tab.temps <- table(temps)
          utemps <- as.numeric(names(tab.temps))
          num <- as.numeric(tab.temps)
          num.list[[i]] <- num
          uniqx.list[[i]] <- utemps
          if(!na.means) {
             my.test.na <- tapply(lt.list[[i]], list(xval.list[[i]]), function(
                x)
             any(is.na(x)))
             utemps.all <- unique(xval.list[[i]])
             utemps.na <- utemps.all[my.test.na]
             if(length(utemps.na) > 0) {
                u.match <- match(utemps.na, utemps, nomatch = 0)
                if(any(u.match > 0))
                   my[u.match] <- NA
             }
             mean.list[[i]] <- my
          }
          if(plot.means)
             points(utemps, my, pch = plotch[i.plot], mkh = mkh, lwd = 2)
          nummin[i] <- min(num[!is.na(my)])
          nummax[i] <- max(num[!is.na(my)])
          sd <- sqrt(vv)
          sem <- sd/sqrt(num)
          df.aov.rss <- sum(num - 1)
          numdf[i] <- df.aov.rss
          ms.aov <- sum(vv * (num - 1), na.rm = T)/df.aov.rss
          vm.aov[i] <- ms.aov
          sd.aov <- sqrt(ms.aov)
          cat("\n", leg[i], "  ", date(), fill = T)
          av.se <- data.frame(rbind(format(round(my, 2)), format(round(sem, 2)),
             num))
          row.names(av.se) <- c("Means are:", "sem's are:", "Nos.")
          names(av.se) <- paste(utemps)
          print(av.se)
          cat("sds are", format(round(sd, 2)))
          if(df.aov.rss > 0)
             cat("  pooled:", format(round(sd.aov, 3)), "  df =", df.aov.rss, 
                fill = T)
          if(log.y) {
             btmeans <- exp(my)
             btsem <- btmeans * sem
             bt.se <- data.frame(rbind(format(round(btmeans, 1)), format(round(
                btsem, 2))))
             row.names(bt.se) <- c("BT means:", "Appr sem:")
             names(bt.se) <- paste(utemps)
             print(bt.se)
          }
          cat("\n", fill = T)
          rtemps <- range(temps)
          utemps <- uniqx.list[[i]]
          poly.i <- poly[i]
          if(poly.i)
             linetype.len <- 3.5
          splin.i <- splin[i]
          if(poly.i & length(utemps) <= deg.i) {
             if(length(utemps) > 1)
                cat("*** Insufficient points for polynomial  of degree", deg.i,
                   fill = T)
             poly.i <- F
          }
          if(splin.i & length(utemps) < 4) {
             if(length(utemps) > 1)
                cat(
                   "*** Insufficient points for spline -- at least 4 are required",
                   fill = T)
             splin.i <- F
          }
          poly.lim <- poly.limits[[i]]
          poly.keep <- poly.keep.x[[i]]
          if(is.null(poly.lim))
             poly.lim <- rtemps
          else if(any(is.na(match(poly.lim, utemps))))
             cat(
                "One or both endpoints for the polynomial fit do not match actual data values",
                fill = T)
          if(length(poly.lim) == 1 & is.null(poly.keep)) {
             cat("Only one limit has been given; cannot fit quadratic", fill = 
                T)
             poly.lim <- rep(poly.lim, 2)
          }
          if(is.null(poly.keep)) {
             keep <- (utemps >= poly.lim[1] & utemps <= poly.lim[2])
             poly.keep <- utemps[keep]
          }
          else keep <- as.logical(match(utemps, poly.keep, nomatch = F))
          nu <- (1:length(utemps))[keep]
          y.means <- my[nu]
          x.means <- utemps[nu]
          yfit <- my
          use.quad <- as.logical(match(temps, poly.keep, nomatch = F))
          x.quad <- temps[use.quad]
          y.quad <- ltval[use.quad]
          num.quad <- num[nu]
          df.quad <- sum(num.quad - 1)
          if(df.quad == 0)
             ms.quad <- 0
          else ms.quad <- sum(vv[nu] * (num.quad - 1), na.rm = T)/df.quad
          ems <- ms.quad
          df.t <- df.quad
          if(poly.i) {
             poly.legend <- "Polynomial"
             if(fit.robust)
                poly.legend <- "Polynomial (robust fit)"
             if(!is.null(poly.keep.x[[i]]))
                cat(poly.legend, "of degree", deg.i, "through points", 
                   poly.keep, fill = T)
             else cat(poly.legend, "of degree", deg.i, "from points", poly.lim[
                   1], "to", poly.lim[2], fill = T)
             uu <- poly.fit(x.quad, y.quad, num.quad, deg.i, ms.quad, df.quad, 
                errors, print.statistics = print.statistics, predict.x = 
                predict.x, fit.robust = fit.robust, wts = wts)
             ypred.se <- uu$pred$se.fit
             df.t <- uu$df.ms
             xtra <- uu$xtra
             ems <- uu$ems
             vm.other[i] <- ems
             df.other[i] <- df.t
             df.t0 <- uu$pred$df
             ems0 <- uu$pred$ms
             if(!is.null(df.t0))
                if(df.t != df.t0)
                   cat("\n ******* df.t =", df.t, "   df.t0 =", df.t0, "\n")
             if(!is.null(ems0)) if(ems != ems0) {
                   cat("****** Warning ******\n")
                   cat("ems =", ems, "   ems0 =", ems0, "\n\n")
                }
### vanilla ms; depends on errors
             b <- uu$b
             if(include.parallel[i]) {
                j.parallel <- j.parallel + 1
                xval.parallel[[j.parallel]] <- x.quad
                lt.parallel[[j.parallel]] <- y.quad
                num.parallel[j.parallel] <- sum(num[nu])
                wts.parallel[j.parallel] <- 1/ems
                df.parallel[j.parallel] <- df.t
             }
             if(!is.null(predict.x)) {
                ypred <- uu$pred$fit
                u.x <- predict.x
                if(!is.null(confidence)) {
                   alpha <- 1 - confidence
                   c.txt <- paste(round(confidence * 100))
                   c.txt1 <- NULL
                   t.stat <- qt(1 - alpha/2, df.t)
                }
                else if(!is.null(compare)) {
                   alpha <- 1 - compare
                   t.stat <- qt(1 - alpha/2, df.t)/sqrt(2)
                   c.txt <- format(round((2 * pt(t.stat, df.t) - 1) * 100, 1))
                   c.txt1 <- paste(round((1 - compare) * 100))
                }
                ci.low <- ypred - ypred.se * t.stat
                ci.high <- ypred + ypred.se * t.stat
                if(log.y) {
                   bt.y <- exp(ypred)
                   bt.sem <- bt.y * ypred.se
                   xx <- data.frame(rbind(format(round(bt.y, 1)), format(signif(
                      exp(ci.low), 3)), format(signif(exp(ci.high), 3)), format(
                      signif(bt.sem, 2))))
                   dimnames(xx) <- list(c("BT preds:", paste(c.txt, "pc low:", 
                      sep = ""), paste(c.txt, "pc high:", sep = ""), 
                      "Approx sem:"), paste(u.x))
                   print(xx)
                   if(!is.null(c.txt1))
                      cat("\nNon-overlap of ci's is equivalent to a test for", 
                         "\nno difference at the", paste(c.txt1, "pc", sep = ""
                         ), "level.\n")
                }
                else {
                   cat("\nTemps   :", format(round(u.x, 1)), fill = T)
                   cat("Pred vals:", format(round(ypred, 1)), fill = T)
                   cat("Appr. sem:", format(round(ypred.se, 2)), fill = T)
                   bt.y <- ypred
                }
             }
             sem[nu] <- sqrt(ems/num.quad)
             if(length(nu) < length(utemps) & errors == "aov") {
                ms.other <- sum(vv[ - nu] * (num[ - nu] - 1), na.rm = T)/sum(
                   num[ - nu] - 1)
                sem[ - nu] <- sqrt(ms.other/num[ - nu])
             }
          }
          if(plot.single.sem)
             errorbars(utemps, my, sem, 0.25 * chw)
          if(poly.i) {
             if(is.null(poly.plot.limits[[i]]))
                xpoints <- pretty(utemps[nu], 50)
             else xpoints <- pretty(poly.plot.limits[[i]], 50)
             yfit[nu] <- rep(b[1], length(nu))
             yhat <- rep(b[1], length(xpoints))
          }
          if(poly.i)
             if(deg.i > 0)
                for(k in 2:(deg.i + 1)) {
                   yfit[nu] <- yfit[nu] + b[k] * utemps[nu]^(k - 1)
                   yhat <- yhat + b[k] * xpoints^(k - 1)
                }
          if(plot.some) {
             if(poly.i & !splin.i)
                lines(xpoints, yhat, lty = i.plot, lwd = lwd)
             if(plot.ci) {
                lines(spline(u.x, ci.low), lty = i.plot)
                lines(spline(u.x, ci.high), lty = i.plot)
             }
          }
          if(splin.i) {
### spline; perhaps polynomial through points between poly.limits
             if(diff(rtemps) > diff(range(utemps[nu]))) cat(
                   "Extend polynomial to the whole range of x-values as a spline curve.",
                   fill = T)
             if(length(utemps) >= 3) {
                uus <- spline(utemps, yfit)
                xpoints <- uus$x
                yhat <- uus$y
                if(length(xtra) > 3) {
                   xtrafit <- approx(uus$x, uus$y, xout = xtra)$y
                   if(log.y)
                      xtrafit <- exp(xtrafit)
                   names(xtrafit) <- paste(xtra)
                   cat("\n Predictions from spline fit:\n")
                   print(round(xtrafit, 2))
                }
                if(plot.some)
                   lines(xpoints, yhat, lty = i.plot)
             }
          }
          if(!is.null(u.x) & !is.null(bt.y))
             outvals[[i.plot]] <- cbind(u.x, bt.y)
       }
### Parallel curve analysis, if requested
### At present, lines are the only option (no plots)
    if(!is.null(parallel)) {
       cat("\nParallel line or curve analysis", fill = T)
       cat("Factors are:", leg.parallel, fill = T)
       deg.here <- max(deg.pol[include.parallel])
       x.all <- unlist(xval.parallel)
       y.all <- unlist(lt.parallel)
       wts.all <- rep(wts.parallel, num.parallel)
       id.parallel <- rep(1:n.parallel, num.parallel)
       f.parallel <- factor(id.parallel, labels = leg.parallel)
       u.parallel <- lm(lt ~ C(stage, treatment) + temp, data = list(lt = y.all,
          temp = x.all, stage = f.parallel, wts = wts.all, deg.here = deg.here),
          weights = wts)
       u.one <- lm(lt ~ temp, data = list(lt = y.all, temp = x.all, stage = 
          f.parallel, wts = wts.all, deg.here = deg.here), weights = wts)    
       print(summary(u.parallel, correl = F))
       print(anova(u.one, u.parallel), test = "F")
    }
    semin <- array(, ngroups)
    semax <- array(, ngroups)
    groupsd <- array(, nlist)
    for(k in 1:ngroups) {
       sublist <- groups[[k]]    ### N.B. May have >1 list item per group
       leg.k <- paste(leg[sublist], collapse = "; ")
       if(!any(include[sublist])) {
          cat("** List nos.", sublist, "have been specified", 
             "\nOne or more of these is not in the include list", fill = T)
          break
       }
       nmin <- min(nummin[sublist])
       nmax <- max(nummax[sublist])
       sd.aov <- sqrt(sum(vm.aov[sublist] * numdf[sublist], na.rm = T)/sum(
          numdf[sublist]))
       sd.other <- sqrt(sum(vm.other[sublist] * df.other[sublist], na.rm = T)/
          sum(df.other[sublist]))
       if(is.na(sd.other))
          next
       if(errors == "aov" & sum(numdf[sublist]) == 0 & poly.i)
          errors.here <- "lack.of.fit"
       else errors.here <- errors
       sd.points <- switch(errors.here,
          aov = sd.aov,
          lack.of.fit = ,
          total = sd.other)
       groupsd[k] <- sd.points
       if(length(sublist) > 1) {
          cat("\nGroup together lists", sublist, fill = T)
          cat("   Overall pooled sd =", format(round(sd.points, 3)), fill = T)
       }
       semax[k] <- sd.points * sqrt(1/nmin)
       semin[k] <- sd.points * sqrt(1/nmax)
       cat("\n", leg.k, ":")
       if(nmax > nmin)
          cat("sem = Max:", format(round(semax[k], 3)), "    Min:", format(
             round(semin[k], 3)), fill = T)
       else cat("sem =", format(round(semax[k], 3)), fill = T)
    }
    if(plot.some) {
       if(plot.groups.sem)
          if(ngroups > 1) {
             se.tmp <- semax
             se.tmp[is.na(se.tmp)] <- 0.55 * chh
             se.last <- se.tmp[ngroups]
             se.two <- se.tmp + c(0, se.tmp[ - ngroups]) + 0.4 * chh
             if(length(legend.linespace) == 1) legend.linespace <- se.two/chh
        
### Can get spacing from parameter legend.linespace by making this a vector
          }
       x.legend <- uxy[1] + diff(uxy[1:2]) * wide.legend + chw
       if(wide.legend == 1)
          x.legend <- uxy[2] - linetype.len * chw - max(nchar(leg)) * chw
       y.legend <- uxy[3] + diff(uxy[3:4]) * high.legend - chh
       if(high.legend == 1) {
          y.legend <- uxy[4] - 0.5 * chh
       }
       if(high.legend == 0)
          y.legend <- uxy[1] + se.last + sum(se.two)
       xpos <- rep(x.legend, length(leg))
       ypos <- y.legend - cumsum(legend.linespace) * chh
    }
    j0 <- 0
    if(plot.groups.sem)
       for(k in 1:ngroups) {
          sd.here <- groupsd[k]
          sublist <- groups[[k]]
          if(legend.sem) {
             j0 <- j0 + 1
             sem.max <- semax[k]
             sem.min <- semin[k]
             errorbars(xpos[j0] - 1.25 * chw, ypos[j0], sem.max, eps = chw/2)
             pretext.pos <- xpos[j0] - 2.5 * chw
             if(!is.na(sem.min) & sem.min < 0.75 * sem.max) {
                errorbars(xpos[j0] - 3 * chw, ypos[j0], sem.min, eps = chw/2)
                pretext.pos <- xpos[j0] - 4.25 * chw
             }
             if(j0 == 1)
                text(pretext.pos, ypos[j0], bars.pretext, adj = 1)
          }
          for(jk in sublist) {
             mv <- mean.list[[jk]]
             num <- num.list[[jk]]
             sem <- sd.here/sqrt(num)
             uniqx <- uniqx.list[[jk]]
             if(!legend.sem)
                errorbars(uniqx, mv, sem, eps = chw/2)
          }
       }
    if(!plot.some)
       return()
    note.pos <- ypos[j0] - 1 * chh
    if(plot.groups.sem)
       note.pos <- note.pos - semax[length(semax)]
    if(nchar(legend.note) > 0)
       text(xpos[1], note.pos, legend.note, adj = 0, cex = 0.75)
    oldmgp <- par(mgp = c(2.75 + ytitladd, 0.5, 0))$oldmgp
    if(names(dev.cur()) == "postscript")
       mixed.mtext(texts = paste(prefix.y, "LT~v~c.8~.", lt, "~h0~c1~. ", ylab,
          sep = ""), side = 2, line = 2.5 + ytitladd, adj = 0.5, cex = 
          cex.labels)
    else mtext(text = paste(prefix.y, "LT", lt, " ", ylab, sep = ""), side = 2,
          line = 2.5 + ytitladd, adj = 0.5, cex = cex.labels)
    par()$oldmgp
    if(length(leg) < nlist) {
       cat("Insufficient legends", fill = T)
       leg <- c(leg, rep("", nlist - length(leg)))
    }
    else if(length(leg) > nlist)
       cat("NB: There are superfluous legends", fill = T)
    if(length(leg) < nlist)
       cat("Insufficient legends", fill = T)
    j0 <- 0
    if(sum(include) > 1) {
       for(i in 1:nlist)
          if(include[i]) {
             j0 <- j0 + 1
             points(xpos[j0], ypos[j0], pch = plotch[j0], cex = 0.9, mkh = mkh)
             if(poly[i])
                lines(c(xpos[j0] + 1.5 * chw, xpos[j0] + (linetype.len + 1.5) * 
                   chw), c(ypos[j0], ypos[j0]), lty = j0)
             if(names(dev.cur()) == "postscript")
                mixed.text(xpos[j0] + (linetype.len + 2) * chw, ypos[j0], leg[i
                   ], adj = 0, cex = 0.8)
             else text(xpos[j0] + (linetype.len + 2) * chw, ypos[j0], leg[i], 
                   adj = 0)
          }
    }
    if(is.null(xlab)) {
       xlab <- ""
       cat("No setting was provided for xlab\n")
    }
    if(names(dev.cur()) == "postscript") {
       mixed.mtext(texts = paste(xlab, sep = ""), side = 1, line = 2.25, adj = 
          0.5, cex = cex.labels)
       if(maint != "")
          mixed.mtext(texts = maint, side = 3, line = 1.75, adj = 0.5, cex = 
             cex.title)
    }
    else {
       mtext(paste(xlab, sep = ""), 1, line = 2.25, cex = cex.labels)
       mtext(text = maint, side = 3, line = 1.75, adj = 0.5, cex = cex.title)
    }
    oldcex <- par(cex = 0.4)
    r.txt <- unix("date +%d/%m/%y'  '%H:%M")
    l.txt <- paste(unix("pwd"), as.character(as.name(match.call())), collapse
        = "")    
### docs <- paste(unix("pwd"), as.character(as.name(match.call())), 
### date.txt, collapse = "")
    if(datestamp) {
       mtext(r.txt, side = 1, adj = 1, outer = T, line = mtext.line)
       mtext(l.txt, side = 1, adj = 0, outer = T, line = mtext.line)
    }
    options(width = 80)    # docs
    invisible()
}
, comment = "08/10/1995")
