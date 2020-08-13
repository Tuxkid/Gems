text.lot2 <- function(outfile = "write.GemFn.src")
{
### Purpose:- Puts Gems function text into its ./.tmp/ directory 
### ----------------------------------------------------------------------
### Modified from:- text.lot
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Aug 2012, 11:52
### ----------------------------------------------------------------------
### Revisions:- 
                                     # creates source file to list all functions
  src.comm <- ppaste("source('", outfile, "')")    # to use src file
  oblist <- ls(pos = 1)
  new.function <- 0    # no of functions to write
  app <- FALSE
  today <- system("date +%d/%m/%Y", TRUE)
  for(i in oblist) {
    date.fn <- comment(get(i))
    if(is.function(get(i))  ) {
      source.line <- ppaste("write.function2(", i, ")")
      write(source.line, file = outfile, append = TRUE)
      new.function <- new.function + 1
     ## app <- TRUE
    }
  }
  cat(ppaste("Now you can write ", new.function, " function",
             ifelse(new.function > 1, "s", "")," using:\n", src.comm,
             "\n"))
}
text.lot2 <-
function(outfile = "write.GemFn.src")
{
### Purpose:- Puts Gems function text into its ./.tmp/ directory 
### ----------------------------------------------------------------------
### Modified from:- text.lot
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Aug 2012, 11:52
### ----------------------------------------------------------------------
### Revisions:- 
                                     # creates source file to list all functions
  src.comm <- ppaste("source('", outfile, "')")    # to use src file
  oblist <- ls(pos = 1)
  new.function <- 0    # no of functions to write
  app <- FALSE
  today <- system("date +%d/%m/%Y", TRUE)
  for(i in oblist) {
    date.fn <- comment(get(i))
    if(is.function(get(i))  ) {
      source.line <- ppaste("write.function2(", i, ")")
      write(source.line, file = outfile, append = TRUE)
      new.function <- new.function + 1
     ## app <- TRUE
    }
  }
  cat(ppaste("Now you can write ", new.function, " function",
             ifelse(new.function > 1, "s", "")," using:\n", src.comm,
             "\n"))
}

