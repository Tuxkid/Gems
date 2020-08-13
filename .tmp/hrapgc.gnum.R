gnum <-
structure(function(file = NULL)
{
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Zaphod Beeblebrox, Date: 15 Apr 2002, 11:26
if(is.null(file))
  system("gnumeric &")
else system(paste("gnumeric",file,"&"))
}
, comment = "28/06/2002")
