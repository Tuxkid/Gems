Put <- function(x, pos = NULL)
{
### Purpose:- Copies function source into ~/Rstuff/Gems/.tmp/
### ----------------------------------------------------------------------
### Modified from:- Get
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  2 Nov 2012, 15:47
### ----------------------------------------------------------------------
### Revisions:- 30/01/2013 added code to save.new, largely redundant

  fun.name <- as.character(substitute(x))
  filedate <- comment(get(fun.name))
  location <- system("pwd", TRUE)
  if(is.null(pos))
    pos <- grep("Gems", search())
  source.dir <- substring(search()[pos], 6) # remove "file:"
  use.source.dir <- gsub("RData", "tmp/hrapgc.", source.dir) # remove .RData
  file.name <- ppaste(use.source.dir, fun.name, ".R")
  system(ppaste("cp ", location, "/.tmp/hrapgc.", fun.name, ".R ", file.name))
}
