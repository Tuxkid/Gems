write.CSV <-
structure(function(x, file, RN = TRUE, append = FALSE, CN = TRUE, ...)
{
### Purpose:- write.csv doesn't allow append
### ----------------------------------------------------------------------
### Modified from:- write.table
###                 RN: row names
###                 CN: column names
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 24 Nov 2010, 14:07
### ----------------------------------------------------------------------
### Revisions:-

  if(CN & RN) { #
    write(paste(",", paste(colnames(x), collapse = ",")), file, append = append)
    write.table(x, file = file, sep = ",", na = "", col.names = FALSE,
                row.names = TRUE, append = append, quote = FALSE)
  } else {
    write.table(x, file = file, sep = ",", na = "", col.names = CN,
                row.names = RN, append = append, quote = FALSE)
  }
}
, comment = "30/11/2010")
write.CSV <-
structure(function(x, file, RN = TRUE, append = FALSE, CN = TRUE, ...)
{
### Purpose:- write.csv doesn't allow append
### ----------------------------------------------------------------------
### Modified from:- write.table
###                 RN: row names
###                 CN: column names
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 24 Nov 2010, 14:07
### ----------------------------------------------------------------------
### Revisions:-

  if(CN & RN) { #
    write(paste(",", paste(colnames(x), collapse = ",")), file, append = append)
    write.table(x, file = file, sep = ",", na = "", col.names = FALSE,
                row.names = TRUE, append = append, quote = FALSE)
  } else {
    write.table(x, file = file, sep = ",", na = "", col.names = CN,
                row.names = RN, append = append, quote = FALSE)
  }
}
, comment = "30/11/2010")
