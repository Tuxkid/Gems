find.first <-
structure(function(x, char = " ")
{
### Finds position of first occurence of a particular character in a character string
    if(!is.character(x)) stop(
          "find.first() applicable only for character strings")
    beg <- NULL
    for(k in 1:length(x)) {
       leg.vec <- paste(substring(x[k], 1:nchar(x[k]), 1:nchar(x[k])), sep = ""
          )
       beg[k] <- match(char, leg.vec)
    }
    beg
}
, comment = "01/06/1996")
