Get <- function(x, pos = NULL)
{
### Purpose:- Copies function source from ~/Rstuff/Gems/.tmp/
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Aug 2012, 13:33
### ----------------------------------------------------------------------
### Revisions:-

  fun.name <- as.character(substitute(x))
  filedate <- comment(get(fun.name))
  location <- system("pwd", TRUE)
  if(is.null(pos))
    pos <- grep("Gems", search())
  source.dir <- substring(search()[pos], 6) # remove "file:"
  use.source.dir <- gsub("RData", "tmp/hrapgc.", source.dir) # remove .RData
  file.name <- ppaste(use.source.dir, fun.name, ".R")
  system(ppaste("cp ", file.name, " ", location, "/.tmp/"))
}
