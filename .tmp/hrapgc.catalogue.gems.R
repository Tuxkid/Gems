catalogue.gems <-
structure(function(x = NULL, outfile = "gems.date.sc", new = TRUE, remove = FALSE)
{
### 
### modifies catalogue() to incorporate dates in Gems functions from Splus
### 
### x is character string of object names of interest. All if not specified
###
    month.name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
       "Sep", "Oct", "Nov", "Dec")
    month.no <- 1:12
    names(month.no) <- month.name
    attach(gems.df)
    on.exit(detach("gems.df"))
    del.comm <- paste("rm", outfile)    # deleting previous src file
    src.comm <- ppaste("source('", outfile, "')")    # to use src file
    if(new)
       try(system(del.comm))
    if(is.null(x))
       x <- ls(pos = 1)
    today <- system("date +%d/%m/%Y", TRUE)
    new.obj <- 0
    for(i in x) {
       yr.i <- Year[Object == i]
       dy.i <- Dy[Object == i]
       mo.i <- month.no[Mo[Object == i]]
       if(length(yr.i) > 0) {
          dy.j <- substring(ppaste("0", dy.i), nchar(dy.i))
          mo.j <- substring(ppaste("0", mo.i), nchar(mo.i))
          today.i <- paste(dy.j, mo.j, yr.i, sep = "/")
          source.line <- ppaste("comment(", i, ") <- '", ifelse(today.i == "", 
             today, today.i), "'")
          if(is.null(comment(get(i)))) {
             write(source.line, file = outfile, append = TRUE)
             new.obj <- new.obj + 1
          }
       }
    }
  if(new.obj > 0)
       cat(paste("Now you can add the date comments to", new.obj, ifelse(
          new.obj > 1, "object", "objects"), "using:\n", src.comm, "\n"))
    else cat(paste("No objects need updating:\n"))
  }
, comment = "04/04/2001")
