add.textB <-
structure(function(xp = 0.5, yp = 0.5) 
{
### Purpose:- Gets basic coordinates a la add.text
### ----------------------------------------------------------------------
### Modified from:- add.text
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 23 Apr 2008, 09:28
### ----------------------------------------------------------------------
### Revisions:- 
  uu <- par()$usr
  xd <- diff(uu[1:2])
  yd <- diff(uu[3:4])
  
  xh <- uu[1] + xd * xp
  yh <- uu[3] + yd * yp
  c(xh, yh)
}
, comment = "23/04/2008")
