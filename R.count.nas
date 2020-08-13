  Listing of: count.nas
  Located in: /home/hrapgc/Rstuff/Gems
Last updated: 01/12/1997 
**************************************

"count.nas" <-
function(df)
{
# counts nas in columns of a dataframe have nas.
#
    sapply(df, function(x)
    sum(is.na(x)))
}
