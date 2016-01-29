text.bp <-
structure(function(x, y.range, width = 45, no.data.string = "NA")
{
### Does a "text boxplot" to use in summarize()

 x <- x[!is.na(x)]
  if(length(x) < 1)
    return(no.data.string)
  x.quan <- (boxplot(x, plot = F)$stats[, 1])
  x.outliers <- boxplot(x, plot = F)$out
#browser()
 if(length(x.outliers)>0)
    out.pos <- round(approx(seq(y.range[1], y.range[2], length = width), 1:
                            width, xout = x.outliers)$y)
  qps <- round(approx(seq(y.range[1], y.range[2], length = width), 1:width, 
                      xout = x.quan)$y)    #
### Have quantile positions...
### Make allowances for overlapping positions of quantiles
                                        #
  qps.start <- qps
  if(prod(diff(qps[-3])) == 0) {
    if(qps[2] - qps[1] < 1)
      qps[2] <- qps[1] + 1
    if(qps[5] - qps[4] < 1)
      qps[4] <- qps[4] - 1
    if(qps[4] - qps[2] < 1) {
      if(qps[5] < width)
        qps[4:5] <- qps[4:5] + 1
      else qps[1:2] <- qps[1:2] - 1
    }
  }
 bar.vec <- rep(" ", width)
 bar.vec[qps[1]] <- "("
 bar.vec[qps[2]] <- "["
 bar.vec[qps[4]] <- "]"
 bar.vec[qps[5]] <- ")"
 if(qps[2] - qps[1] > 1)
    bar.vec[(qps[1] + 1):(qps[2] - 1)] <- "."
 if(qps[4] - qps[2] > 1)
    bar.vec[(qps[2] + 1):(qps[4] - 1)] <- "-"
 if(qps[5] - qps[4] > 1) bar.vec[(qps[4] + 1):(qps[5] - 1)] <- "."    #
### Add in outliers if present
 if(length(x.outliers)>0)
   bar.vec[out.pos] <- "*"
 bar.tex <- paste(bar.vec, collapse = "")
 full.bar.tex <- paste(":", bar.tex, ":", sep = "")
 full.bar.tex
}
, comment = "18/06/2001")
