unfactor <-
structure(function(x, cols = "every.factor.column", notify = F)
{
#Converts factors back to numerics or character vectors
#
# A dataframe (or vector) of the same dimensions and names is returned
# Default is to change all factor columns 
# Alternatively, specific columns can be specified
#
# notify:  Notify that no factors need changing? 
# Vectors have to be handled differently:
    onecol <- is.null(dim(x))
    if(is.null(dim(x))) {
       x <- as.data.frame(x)
       cols <- 1
    }
# Handling data frames:
    if(is.numeric(cols))
       change.cols <- cols
    else {
       if(cols == "every.factor.column") {
          fact.cols <- check.fact(x)
          change.cols <- find.pos(T, fact.cols)    #
# (a numerical vector of column numbers that are to be changed)
       }
       else change.cols <- find.pos(cols, names(x))
    }
    if(length(change.cols) == 0 & notify)
       cat(paste("No factors to change in", as.character(substitute(x)), "\n"))
    else {
       options(warn = -1)
       on.exit(options(warn = 0))
       for(i in change.cols) {
          levels.i <- levels(x[[i]])
          level.nos <- as.numeric(levels.i)
          numbers <- ifelse(any(is.na(level.nos)), F, T)
          if(numbers)
             x[, i] <- as.numeric(I(as.character(x[, i])))
          else x[, i] <- I(as.character(x[, i]))
       }
    }
    if(onecol)
       x <- as.vector(unlist(x))
    x
}
, comment = "30/04/2001")
