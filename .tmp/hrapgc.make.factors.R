make.factors <-
structure(function(df, fcols = 1, ordered = F, ...)
{
### To change specified columns into factors.  By default levels are reordered 
###    in alphabetical order.  Keeping that order makes it an ordered factor (ordered = T)
###    which does some strange things in glms and the like, but it's useful for ordering 
###    factors for trellis plotting.
###
###  Without named columns, will not work unless fcols are sequential (col nos. are used:
###    somehow that makes a difference)
###
### Vectors can also be used, in which case setting sorted to T is the same
###   as using factor()
###
    onecol <- is.null(dim(df))    #
    if(onecol) {
       vec <- unfactor(df)
       if(!ordered)
          df <- factor(vec, ...)
       else df <- ordered(vec, labels = unique(vec, ...))
    }
    else {
       if(is.character(fcols))
          fcols <- find.pos(fcols, names(df))
       for(i in fcols) {
          if(!ordered) {
             df[[i]] <- factor(df[[i]], ...)
          }
          else df[[i]] <- ordered(df[[i]], levels = unique(df[[i]]), ...)
       }
    }
    df
}
, comment = "06/01/1998")
