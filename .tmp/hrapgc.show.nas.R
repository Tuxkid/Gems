show.nas <-
structure(function(df)
{
### identifies which columns of a dataframe have nas.
#
    sapply(df, function(x)
    any(is.na(x)))
}
, comment = "01/12/1997")
