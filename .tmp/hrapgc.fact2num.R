fact2num <-
structure(function(x)
{
### Purpose:- Converts a factor into a numeric (avoiding characters)
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 15 Jan 2010, 09:13
### ----------------------------------------------------------------------
### Revisions:-

as.numeric(levels(x)[x])
  
}
, comment = "15/01/2010")
