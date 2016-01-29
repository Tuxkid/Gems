initEstB <-
structure(function(xx, yy, Class = c("Ma", "Pa"), blup.mat = NULL)

{
### Purpose:- get initial values for random and rcov variance estimates
###   to supply to asreml for multi-variate analysis.
### ----------------------------------------------------------------------
### Modified from:- initEst (which is mostly Peter's function)
### ----------------------------------------------------------------------
### Arguments:- xx: dataframe (or matrix) with data
###             yy: columns to be analysed together
###             Class: names of columns that identify groups
###             blup.mat: matrix of BLUPs used instead of raw values
### ----------------------------------------------------------------------
### Author:-  Patrick Connolly, Date:- 10 Feb 2008, 02:08 
  xx <- xx[, c(Class, yy)]
  rcovInit <- cov(xx[, yy], use = 'complete')
  tt <- aggregate(xx[, yy], xx[, Class], mean, na.rm = TRUE)
  ## Get univariate BLUPs from univariate analysis
  if(is.null(blup.mat)) # we're using raw values for initial
    randomInit <- cov(tt[, yy], use = 'complete')
  else randomInit <- cov(blup.mat, use = 'complete')# using univariate BLUPs 
  rcovInit <- rcovInit - randomInit
  if (any(diag(rcovInit) < 0))
    warning('Negative variance estimate(s) in initial estimates for rcov')
  rcovInit <- rcovInit[upper.tri(rcovInit, diag = TRUE)]
  randomInit <- randomInit[upper.tri(randomInit, diag = TRUE)]
  list(randomInit = randomInit, rcovInit = rcovInit)
}
, comment = "20/02/2008")
