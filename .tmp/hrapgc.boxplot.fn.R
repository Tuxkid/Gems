boxplot.fn <-
structure(function(x = delbbd.datf, floop = NULL, rloop = NULL, trans = function(z)
z, mod1 = 1, mod2 = 0, notch = F, subset = NULL, paste.cols = NULL)
{
# 1a) Adding Paste Columns
    if(!is.null(paste.cols) & !is.logical(paste.cols)) {
       indc <- apply(x[paste.cols], 1, paste, collapse = ":")    
    #ind <- match(indc, unique(indc))
       x <- cbind(indc, x)
       dimnames(x)[[2]][1] <- paste("treat", paste(paste.cols, collapse = ""), 
          sep = "")
       if(is.null(floop)) {
          floop <- 1
       }
       else {
          floop <- c(1, floop + 1)
       }
    }
# 1b) For loop for Responses
    for(j in rloop) {
       response.names <- dimnames(x)[[2]][j]
       response <- x[, j]
       if(!is.null(subset)) {
          response[response > subset] <- NA
       }
       yval <- trans(response/mod1 + mod2)    # 2) For loop for factors
       for(i in floop) {
          factors <- factor(x[, i])
          factor.names <- dimnames(x)[[2]][i]    # 3) Boxplot function
          plot.factor(factors, yval, xlab = factor.names, ylab = response.names,
             notch = notch, boxmeans = T, style.bxp = "old")
          if(i == floop[1] & j == rloop[1])
             mtext(paste(as.character(substitute(trans)), " ", as.character(
                substitute(x))), 3, cex = 1, line = 1.3999999999999999)
          dev.ask()
       }
    }
}
, comment = "06/12/1996")
