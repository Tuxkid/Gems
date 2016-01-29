subsets <-
structure(function(n, r, v = 1:n) # works in S or R
{
  ## Purpose: Gets permutations of vectors
  ## ----------------------------------------------------------------------
  ## Modified from: Rnews (Vol 2 I think)
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Bill Venables , Creation date:  6 Jun 2000
  if(r <= 0) vector(mode(v), 0) else
  if(r >= n) v[1:n] else {
    rbind(cbind(v[1], Recall(n-1, r-1, v[-1])),
          Recall(n-1, r, v[-1]))
  }
}
, comment = "06/06/2000")
