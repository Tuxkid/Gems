sublet <-
structure(function(x = "dog")
{
### Purpose:- subsets a character string into vector of single characters
###   then tries every combination of possible single letter that could
###   be added to make a word.
### ----------------------------------------------------------------------
### Modified from:-
### ----------------------------------------------------------------------
### Arguments:-
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  1 Apr 2014, 14:10
### ----------------------------------------------------------------------
### Revisions:-

  lets <- NULL
  for(i in seq(nchar(x)))
    lets <- c(lets, substring(x, i, i))
  lets # letters we begin with
  out2 <- NULL
  for(j in letters){# any letter can be tried
    xj <- c(j, lets)
    xjr <- rev(xj)
    out2 <- c(out2, paste(xj, collapse = ""), paste(xjr, collapse = ""))
    for(k in 1:length(xj)){# various arrangements of those letters
      xjk <- xj[c((k + 1):length(xj), 1:k)]
      out2 <- c(out2, paste(xjk, collapse = ""))
      xjkr <- xjr[c((k + 1):length(xjr), 1:k)]
      out2 <- c(out2, paste(xjkr, collapse = ""))
    }
  }
##  fish out the ones that don't have NAs
aaa <- out2[nchar(out2) == length(lets) + 1]
  factor(aaa) # make a factor to eliminate quotes

}, comment = "01/04/2014")
