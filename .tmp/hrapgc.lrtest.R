lrtest <- function(x = dipFF_16.h, y = dipFF_16.v)
{
### Purpose:- Likelihood ratio test for two asreml models
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- x, y lists produced by Asreml.vA type functions
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 20 Mar 2014, 13:19
### ----------------------------------------------------------------------
### Revisions:-
  D <- 2 * (x$LL - y$LL) # likelihood difference
  if(D < 0)
    stop("Maybe try swapping x and y\n")
  df <- nrow(x$Vcomp) - nrow(y$Vcomp) # difference in No parameters estimated
  pchisq(D, df) # probability of that difference being random
}
