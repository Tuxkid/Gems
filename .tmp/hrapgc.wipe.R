wipe <-
structure(function (x) 
{
  ## Purpose:  Removes the file in /tmp/ and sets source attribe to NULL
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Zaphod Beeblebrox, Date:  2 Mar 2002, 17:09
  ob.name <- deparse(substitute(x))
  attr.com <- ppaste("attr(", ob.name, ", \"source\") <- NULL\n")
#### 26/6/07 modify to use new position of tmp directory
  tmp.com <- ppaste("rm .tmp/hrapgc.", ob.name, ".R\n")
  try(system(tmp.com))
  write.com <- ppaste("write.function(", ob.name, ")")
  cat("  If you want to keep the code, now run these commands:\n")
  cat(paste(write.com, attr.com, sep = " ;"))
}
, comment = "26/06/2007")
