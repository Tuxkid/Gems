truncate.list <-
structure(function(x, y)
{
### Purpose:- Truncates a list by specifying names to be removed
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- x: a list
###             y: vector of names of elements to ditch
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 19 Aug 2010, 11:36
### ----------------------------------------------------------------------
### Revisions:-

  x[jettison(y, names(x))]
}
, comment = "19/08/2010")
