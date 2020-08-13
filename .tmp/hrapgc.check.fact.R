check.fact <-
structure(function(df)
{
#Checks if columns of dataframes are factors:
    sapply(df, is.factor)
}
, comment = "30/04/2001")
