gmt <-
structure(function(x, format = "%Y-%m-%d")
{
### Purpose: Converts a text string of yyyy-mm-dd format to POSIXct at tz = GMT
### ----------------------------------------------------------------------
### Modified from: (useful to use with ddif() to get days difference
### ----------------------------------------------------------------------
### Arguments: modify format for zillions of different date formats
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 20 Feb 2004, 09:41
  
x1 <- strptime(x, format = format)
 as.POSIXct(x1, tz = "GMT")


}
, comment = "05/03/2004")
