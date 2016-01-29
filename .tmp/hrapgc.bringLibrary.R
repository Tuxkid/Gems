bringLibrary <-
  function(lastR = "3.0.2", latestR = "3.1.0", Rloc = "/usr/local/")
{
### Purpose:- Bring library from older R version to newer one.
###           (Idea is to then update the packages that need to be)
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- lastR: version of R that has the packages desired
###             latestR: latest version of R of interest
###             Rloc: where R versions are located (probably not the default)
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 May 2014, 11:08
### ----------------------------------------------------------------------
### Revisions:- 22/08/14 fixed mistaken swap of lastR & latestR
###             15/10/15 location of R installions made adjustable
  now <- system(paste0("ls ", Rloc, "R-", latestR, "/library"), TRUE)
  was <- system(paste0("ls ", Rloc, "R-", lastR, "/library"), TRUE)
  need <- was[!is.element(was, now)]
### Check if it's already been done
  if(length(need) < 1)
    stop("Nothing in R-", lastR, " that isn't already in R-", latestR, ".\n")
  for(i in need) # reason for running this function
    system(paste0("cp -prv ", Rloc, "R-", lastR, "/library/", i, " ", Rloc,
                  "R-", latestR, "/library/"))
### Notify it's finished and give pastable text to update copied packages 
  cat(length(need), " packages copied into R-", latestR,
      " directory.\nProbably a good idea to start R-", latestR,
      " and run\n  update.packages(checkBuilt = TRUE, ask = FALSE)\n", sep = "")
}
