tab2df <-
structure(function(x)
{
### Purpose:- makes a table into a dataframe
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- x is a table object
### ----------------------------------------------------------------------
### Author:-   Found on internet, Date:- 30 Mar 2010, 16:41
### ----------------------------------------------------------------------
### Revisions:- 
  columns <- list()
  rows <- row.names(x)
  for(colname in colnames(x))
    columns[[colname]] <- x[,colname]
  df <- data.frame(columns)
  row.names(df) <- rows
  df


}
, comment = "30/03/2010")
