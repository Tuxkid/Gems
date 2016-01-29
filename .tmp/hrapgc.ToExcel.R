ToExcel <-
structure(function(dfs = ls(patt = "df", pos = 1), groupid = "", out = "out",
           rm.csv = TRUE, RN = TRUE)
{
  ## Purpose: Excel files from the data frames
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##           dfs: which dataframes do we want
  ##           groupid: Character to distinguish from other groups of csv files
  ##           out: prefix to the Excel file name
  ##           RN: do we want row names?
  ##           rm.csv: logical -- are we deleting the CSV files?
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 26 Sep 2007, 11:11
  ## ----------------------------------------------------------------------
  ## Revisions:- 27/10/11: copied RN argument from ToExcelF

  for(i in dfs){
    csv <- ppaste(groupid, sub("df", "CSV", i))
    write.csv(get(i, parent.frame()), csv, quote = FALSE,
              na = "", row.names = RN)
  }
  out.xls <- ppaste(out, ".xls")
  make.excel <- ppaste("WriteExcel.pl --CSVfiles=", groupid, "*CSV ", out.xls)
  system(make.excel)
  if(rm.csv) system(ppaste("rm ", groupid, "*CSV"))

}
, comment = "27/10/2011")
