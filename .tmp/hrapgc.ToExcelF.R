ToExcelF <-
structure(function(patt = "df", dfs = NULL, groupid = "", out = patt,
           rm.csv = TRUE, RN = TRUE)
{
### Purpose:- Use within a function that creates its dataframes
### ----------------------------------------------------------------------
### Modified from:- ToExcel
### ----------------------------------------------------------------------
### Arguments:- RN: do we want row names?
###             patt: pattern to use if dfs is not specified
###             dfs: names of dataframes required in Excel file
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 25 May 2009, 16:11
### ----------------------------------------------------------------------
### Revisions:-
  if(is.null(dfs))
    dfs <- ls(patt = patt, parent.frame()) 
  for(i in dfs){
    ii <- get(i, parent.frame())
    if(!is.data.frame(ii))
       stop(ppaste(i, " is not a dataframe:\n"))
    if(grep("df", i) > 0)
      csv <- ppaste(groupid, sub("df", "CSV", i))
    else csv <- ppaste(groupid, i, "CSV")
    write.csv(ii, csv, quote = FALSE, na = "", row.names = RN)
  }
  out.xls <- ppaste(out, ".xls")
  make.excel <- ppaste("WriteExcel.pl --CSVfiles=", groupid, "*CSV ", out.xls)
  system(make.excel)
  if(rm.csv) system(ppaste("rm ", groupid, "*CSV"))
}
, comment = "29/07/2009")
