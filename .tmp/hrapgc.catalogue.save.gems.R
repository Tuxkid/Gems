catalogue.save.gems <-
structure(function(x = NULL, outfile = "gems.date.save.sc", new = TRUE)
{
### 
### modifies catalogue() to incorporate dates in Gems functions from Splus
### 
### x is character string of object names of interest. All if not specified
###

  attach(gem.df)
  on.exit(detach("gem.df"))
  del.comm <- paste("rm", outfile)    # deleting previous src file
  src.comm <- ppaste("source('", outfile, "')")    # to use src file
  if(new)
    try(system(del.comm))
  if(is.null(x))
    x <- ls(pos = 1)
  
  today <- system("date +%d/%m/%Y", TRUE)
  new.obj <- 0
  for(i in x) {
    date.i <- Date[Object == i]
source.line <- ppaste("comment(", i, ") <- '", date.i, "'")
     
      if(is.null(comment(get(i)))) {
        write(source.line, file = outfile, append = TRUE)
        new.obj <- new.obj + 1
      }
  }

  if(new.obj > 0)
    cat(paste("Now you can add the date comments to", new.obj, ifelse(
                                                                      new.obj > 1, "object", "objects"), "using:\n", src.comm, "\n"))
  else cat(paste("No objects need updating:\n"))
}
, comment = "31/05/2001")
