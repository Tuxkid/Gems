df.mat <-
structure(function(df)
{
# Coerces a dataframe to a matrix, retaining levels instead of codes 
#  for any factors: see unfactor()
    mat <- as.matrix(unfactor(df))
    mat
}
, comment = "18/10/1996")
