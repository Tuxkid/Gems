getbit <- function(x = "good-stuff", on = "-", bit = 1)
{
### Purpose:- get a specified part after splitting a text string
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:-
###    x: a vector containing text string elements
###    on: string to use for splitting
###    bit: which bit of the string
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  1 Sep 2014, 11:15
### ----------------------------------------------------------------------
### Revisions:-
  sapply(strsplit(x, on), "[", bit)
}
