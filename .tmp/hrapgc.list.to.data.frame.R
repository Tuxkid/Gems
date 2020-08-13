list.to.data.frame <-
structure(function(x)
{
# 1) Test list element lengths are the same
    lengths <- unlist(lapply(x, length))
    len <- mean(lengths)
    if(all(lengths == len)) {
       attr(x, "class") <- "data.frame"
       attr(x, "row.names") <- 1:len
    }
    else {
       stop("list elements are not of same length")
    }
    x
}
, comment = "19/04/1997")
