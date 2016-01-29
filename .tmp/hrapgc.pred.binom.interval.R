pred.binom.interval <-
function(zz, lnfn = logit.bt, rnd = 1)
{
### Purpose:- Predicted confidence interval for binomial prediction object
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- zz: a prediction object
###             link: backtransforming link as in GLM used in prediction object
###             rnd: rounding
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 17 Apr 2013, 12:18
### ----------------------------------------------------------------------
### Revisions:-
  mean.i <- round(lnfn(zz$fit)*100, rnd)
  seU.i <- round(lnfn(zz$fit + zz$se.fit * 2)*100, rnd)
  seL.i <- round(lnfn(zz$fit - zz$se.fit * 2)*100, rnd)
  outPred <- ppaste(mean.i, " (", seL.i, "-", seU.i, ")")
  outPred
}
