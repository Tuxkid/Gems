  Listing of: show.nas
  Located in: /home/hrapgc/Rstuff/Gems
Last updated: 01/12/1997 
**************************************

"show.nas" <-
function(df)
{
### identifies which columns of a dataframe have nas.
#
    sapply(df, function(x)
    any(is.na(x)))
}
