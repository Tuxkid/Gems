gen.write <-
structure(function(df, file)
{
#To create a file readable by genstat from a dataframe
#
  write.table(df, file, sep = "  ", na = "*", row.names = F, col.names = F, quote = F)
}
, comment = "26/03/2003")
