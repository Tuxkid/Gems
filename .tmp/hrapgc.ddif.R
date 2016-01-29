ddif <-
structure(function(d1, d2)
{
### Purpose: Finds the number of days between two different dates
### ----------------------------------------------------------------------
### Modified from: 
### ----------------------------------------------------------------------
### Arguments: d1 and d2 must be POSIXct objects with tz = "GMT"
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 20 Feb 2004, 09:09
d1s <- as.numeric(d1) # number of seconds since origin
d2s <- as.numeric(d2) # 
(d1s - d2s)/24/3600

}
, comment = "20/02/2004")
