jettison <-
structure(function(x = "a", from = letters)
{
### Purpose:- Jettison 'x' from 'from'
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- x: character vector
###             from: (usually) longer one from which x is to to be removed
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 17 Aug 2010, 12:04
### ----------------------------------------------------------------------
### Revisions:-
  from[!from%in%x]
}
, comment = "17/08/2010")
