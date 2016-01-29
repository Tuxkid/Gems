get.col.names <-
structure(function(x, patt = ".D")
{
# Gets the names of x that fit a particular pattern patt
    col.names <- names(x)
    y <- col.names[grep(patt, col.names)]
}
, comment = "13/06/2000")
