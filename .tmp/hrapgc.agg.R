agg <-
structure(function(data, by, FUNS)
{
#
#   DATE WRITTEN:  14 May 1998
#   AUTHOR:  John R. Wallace (jrw@fish.washington.edu)
#
    out <- aggregate.data.frame(data, by, eval(parse(text = FUNS[1])))
    for(i in 2:length(FUNS)) {
       tmp <- aggregate.data.frame(data, by, eval(parse(text = FUNS[i])))
       tmp <- tmp[, ncol(tmp)]
       out <- cbind(out, tmp)
    }
    dimnames(out)[[2]][ - (1:length(by))] <- FUNS
    out
}
, comment = "15/09/1998")
