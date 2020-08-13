make.df <-
structure(function(text.file, ...)
{
# Making a dataframe from a text file and using tab separators
    read.table(text.file, T, as.is = T, sep = "\t", row.names = NULL, ...)
}
, comment = "01/04/1997")
