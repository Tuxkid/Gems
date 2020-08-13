catalogue <-
function(x = NULL, outfile = "add.dates.sc", new = TRUE, remove = FALSE)
{
### by default, adds today's date as a comment on any object that has none.
### new: delete any previous src file?
### src.now: delete any previous src file?
### remove: delete this src file?
### 31/01/2013 added in ignore.stdout = TRUE, ignore.stderr = TRUE
    del.comm <- paste("rm", outfile)    # deleting previous src file
    src.comm <- ppaste("source('", outfile, "')")    # to use src file
    if(new)
       try(system(del.comm, ignore.stdout = TRUE, ignore.stderr = TRUE))
    if(is.null(x))
       x <- ls(pos = 1)
    new.obj <- 0
    today <- system("date +%d/%m/%Y", TRUE)
    for(i in x) {
       source.line <- ppaste("comment(", i, ") <- '", today, "'")
       if(is.null(comment(get(i)))) {
          write(source.line, file = outfile, append = TRUE)
          new.obj <- new.obj + 1
       }
    }
    if(remove)
       system(del.comm, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if(new.obj > 0)
       cat(paste("Now you can add the date comments to", new.obj, ifelse(
          new.obj > 1, "objects", "object"), "using:\n", src.comm, "\n"))
    else cat(paste("No objects need updating:\n"))
}
