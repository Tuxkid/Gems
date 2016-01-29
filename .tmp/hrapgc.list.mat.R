list.mat <-
structure(function(x)
{
# To convert a list of vectors into a matrix
#
# x must be a named list of "named vectors"
    rownames <- names(x)
    colnames <- names(x[[1]])
    mat <- matrix(nr = length(rownames), nc = length(colnames), dimnames = list(
       rownames, colnames))
    for(i in rownames)
       mat[i,  ] <- unlist(x[[i]])
    mat
}
, comment = "01/02/1997")
