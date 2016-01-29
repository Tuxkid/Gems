qik.sum <-
structure(function(df, cols = 1:4, ordered = F)
{
### Does a quick summary, first coercing specified cols to factors and making a matrix
###  into a dataframe.
#
### Requires make.factors() also
    df <- as.data.frame(df)
    summary(make.factors(df, cols, ordered = ordered))
}
, comment = "13/08/1998")
