monotone <- function(x = c(1, 3, 6, 7, 5, 8), decreasing = FALSE)
{
### Purpose:- takes out "dips" in an onstensibly monotonic vector
### ----------------------------------------------------------------------
### Modified from:-
### ----------------------------------------------------------------------
### Arguments:- decreasing: is x to become monotonic decreasing?
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  9 Sep 2011, 11:12
### ----------------------------------------------------------------------
### Revisions:-
if(!is.numeric(x))
  stop("Works only with numeric vectors\n")
  if(decreasing)
    x <-  rev(x)
  x.x <- diff(x)
  while(any(x.x < 0)){
    dip.at <- which(x.x < 0)
    for(i in dip.at)
      x[i + 1] <- x[i]
    x.x <- diff(x)
  }
  if(decreasing)
    x <-  rev(x)
  x
}
