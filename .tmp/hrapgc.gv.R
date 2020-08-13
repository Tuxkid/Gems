gv <-
structure(function(file = NULL)
{
  ## Purpose:  Starts Ghostview in the current directory
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Zaphod Beeblebrox, Date: 21 Mar 2002, 13:18
if(is.null(file))
  system("gv &")
else system(paste("gv",file,"&"))

}
, comment = "28/06/2002")
