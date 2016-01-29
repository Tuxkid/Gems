check.na <-
structure(function(df)
{
# Returns the number of NAs in any column of a dataframe
    sapply(df, function(x)
    sum(is.na(x)))
}
, comment = "21/09/1997")
