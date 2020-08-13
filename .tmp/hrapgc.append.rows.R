append.rows <-
structure(function(x, id.cols = 1, spread.col = 3, spread.id = 2,
                        new.cols = NULL)
{
### Purpose: Unstacks dataframe -- converse of append.cols
### ----------------------------------------------------------------------
### Modified from: append.cols (a bit)
### ----------------------------------------------------------------------
### Arguments: new.cols: names of new columns
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 20 Feb 2003, 09:28

  x <- as.data.frame(x)
### more useful in character form
  if(is.numeric(spread.col))
    spread.col <- names(x)[spread.col]
  if(is.numeric(spread.id))
    spread.id <- names(x)[spread.id] # column identifier to get new names
  if(is.numeric(id.cols)) 
    id.cols <- names(x)[id.cols] # rows that identify general groups
  if(is.null(new.cols))
    new.cols <- unique(x[, spread.id])

  nrow.out <- nrow(x)/length(new.cols)
### set up dataframe and then add columns to it from rows of original
  x <- df.sort(x, spread.id) # make sure it's ordered
  out.df <- as.data.frame(x[seq(nrow.out), id.cols])
  row.names(out.df) <- seq(nrow(out.df))
  names(out.df) <- id.cols
  spread.mat <- matrix(x[, spread.col], ncol = length(new.cols))
  dimnames(spread.mat) <- list(NULL,new.cols)
  cbind(out.df, spread.mat)
}
, comment = "20/02/2003")
