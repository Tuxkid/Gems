cols.for.genstat <-
structure(function(x, num = F)
{
# Returns a string of the column names to use in genstat
# need only to change the opening and closing " and  to '
    y <- names(x)
    paste(y, collapse = ", ")
}
, comment = "12/04/1999")
