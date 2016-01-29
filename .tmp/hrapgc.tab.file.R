tab.file <-
structure(function(x, file, rounding = 3, ...)
{
### To write a dataframe or matrix to a tab delimited file
    if(!is.null(rounding)) {
       if(is.data.frame(x))
          x <- as.matrix(x)
       x <- round(x, rounding)
    }
    write.table(x, file, sep = "\t", ...)
}
, comment = "25/12/1997")
