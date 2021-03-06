dmodes.f <-
structure(function(t.ob = NULL, pos = 1, max = NULL, date.sort = TRUE)
{
### Purpose: lists only the functions
### ----------------------------------------------------------------------
### Modified from: dmodes
### ----------------------------------------------------------------------
### Arguments: 
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 10 Sep 2002, 09:55
### Returns a Data frame showing modes of objects in the stated directory.
### Can be sorted by Mode, Length, or Date in addition to default Object
### A variation on this one is modes(). Made for lazy typists to sort by object name 
### max is the maximum number of objects returned; useful if ls() is long and 
###  you're only interested in the first few
  if(is.null(t.ob)) ls.o <- objects(pos = pos, all.names =FALSE)
  else ls.o <- objects(pos = pos, pattern = t.ob, all.names =FALSE)
  if(!is.null(max) && max > length(ls.o))
    print(paste("Only", length(ls.o), "match", t.ob, "in", search()[pos]))
  if(is.null(max) || (max > length(ls.o))) max <- length(ls.o)    #
### Rearrange date information
  date.chr <- NULL
  for(i in seq(ls.o)) {
    date.i <- comment(get(ls.o[i], pos = pos))
    if(is.null(date.i))
      date.i <- "NA/NA/NA"
    date.chr[i] <- date.i
  }
  break.at <- occurrence(date.chr, "/", 1:2)
  if(is.null(dim(break.at)))
    break.at_matrix(break.at,nrow=1)
  first <- break.at[, 1] - 1
  second <- break.at[, 2] - 1
  day <- as.numeric(substring(date.chr, 1, first))
  month <- as.numeric(substring(date.chr, first + 2, second))
  year <- as.numeric(substring(date.chr, second + 2))
  ob.df <- unfactor(data.frame(Object = ls.o, Day = day, Mon = month, Year = 
                               year))
  ob.df <- df.sort(ob.df, "Object", backwards = TRUE)
  if(all(is.na(ob.df$Day)))
    date.sort <- FALSE ## no point trying to sort
  if(date.sort) {
    xx <- df.sort(ob.df, c("Day","Mon"))
    xx <- df.sort(xx, "Year", backwards = TRUE)[seq(max),  ]
  }
  else xx <- df.sort(ob.df, "Object")
  xx$Date <- date.chr[as.numeric(row.names(xx))]
  options(warn = -1)
  on.exit(options(warn = 0))
  out <- list()
  dummy <- matrix(nc = 3, nr = 3)    # a non-null last list element
  out$Object <- c(xx$Object, "dummy")
  objs.mat <- cbind(out$Object)
  out$Mode <- apply(objs.mat, 1, function(X)
                    mode(get(X)))    #
### Dimension information of dataframes and matrices
  dimensions <- apply(objs.mat, 1, function(X)
                      dim(get(X))[1:2])
  y <- dimensions
  if(!is.null(y)) {
    if(is.list(y)) {
      y[unlist(lapply(y, is.null))] <- list(c(NA, NA))
      z <- t(matrix(unlist(y), byrow = F, nc = length(y)))
      z[is.na(z)] <- "--"
      out$Rows <- z[, 1]
      out$Cols <- z[, 2]
    }
    else {
### otherwise it's a matrix which will work differently (not checked in R version)
      out$Rows <- y[1,  ]
      out$Cols <- y[2,  ]
    }
  }

  else out$Cols <- out$Rows <- rep("--", length(out$Object))    #  
  out$Len <- apply(objs.mat, 1, function(X)
                   length(get(X)))    #  browser()
  out$Date <- xx$Date
  dfobj <- apply(objs.mat, 1, function(X)
              is.data.frame(get(X)))    #
  out$Mode[dfobj] <- "dataframe"
  out.df <- unfactor(as.data.frame(lapply(out, function(X)
                                          X[seq(max)])), c("Object", "Mode"))
  
  out.df <- out.df[out.df$Mode == "function", ]
  df.to.file(out.df, row.nos = T)
}
, comment = "10/09/2002")
