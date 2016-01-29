gems <-
structure(function(ww = "plot", most = 10)
{
  ## Purpose: searches Gems for function using dmodes
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: 
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 17 Jun 2004, 13:56

gem.pos <- grep("Gems", search())
dmodes(ww, gem.pos, most)

}
, comment = "17/06/2004")
