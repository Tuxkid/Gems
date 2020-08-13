gimp <-
structure(function(file = NULL)
{
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Zaphod Beeblebrox, Date: 15 Apr 2002, 11:26
if(is.null(file))
  system("gimp &")
else system(paste("gimp",file,"&"))
}
, comment = "15/04/2002")
