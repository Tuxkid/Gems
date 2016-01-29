count.nas <-
structure(function(df)
{
# counts nas in columns of a dataframe have nas.
#
    sapply(df, function(x)
    sum(is.na(x)))
}
, comment = "01/12/1997")
