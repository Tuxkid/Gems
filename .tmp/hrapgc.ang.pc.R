ang.pc <-
structure(function(u)
{
### Purpose:- Calculates the angular transform of PERCENTAGE data
### ----------------------------------------------------------------------
### Modified from:- ang (divided input by 100)
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-  Patrick Connolly, Date:- 24 Jan 2008, 11:22
  (180 * asin(sqrt(u/100)))/atan(1)/4
}
, comment = "24/01/2008")
