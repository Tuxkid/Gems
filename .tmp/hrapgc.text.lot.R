text.lot <-
function(outfile = "write.fn.src")
{
                                        # creates source file to list all functions
  src.comm <- ppaste("source('", outfile, "')")    # to use src file
  oblist <- ls(pos = 1)
  new.function <- 0    # no of functions to write
  app <- FALSE
  today <- system("date +%d/%m/%Y", TRUE)
  for(i in oblist) {
    date.fn <- comment(get(i))
    if(is.function(get(i)) & (date.fn == today | date.fn == "") ) {
      source.line <- ppaste("write.function(", i, ")")
      write(source.line, file = outfile, append = app)
      new.function <- new.function + 1
      app <- TRUE
    }
  }
  cat(ppaste("Now you can write ", new.function, " function",
             ifelse(new.function > 1, "s", "")," using:\n", src.comm,
             "\n"))
}

text.lot <-
structure(function(outfile = "write.fn.src")
{
                                        # creates source file to list all functions
  src.comm <- ppaste("source('", outfile, "')")    # to use src file
  oblist <- ls(pos = 1)
  new.function <- 0    # no of functions to write
  app <- FALSE
  today <- system("date +%d/%m/%Y", TRUE)
  for(i in oblist) {
    date.fn <- comment(get(i))
    if(is.function(get(i)) & (date.fn == today | date.fn == "") ) {
      source.line <- ppaste("write.function(", i, ")")
      write(source.line, file = outfile, append = app)
      new.function <- new.function + 1
      app <- TRUE
    }
  }
  cat(ppaste("Now you can write ", new.function, " function",
             ifelse(new.function > 1, "s", "")," using:\n", src.comm,
             "\n"))
}
, comment = "04/02/2003")
