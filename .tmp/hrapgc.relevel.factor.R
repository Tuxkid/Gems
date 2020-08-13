relevel.factor <-
structure(function(x, ref, ...)
{
    lev <- levels(x)
    nlev <- length(lev)
    if(is.character(ref))
       ref <- match(ref, lev)
    if(is.na(ref))
       stop("ref must be an existing level")
    if(ref < 1 || ref > nlev)
       stop(paste("ref =", ref, "must be in 1 :", nlev))
    factor(x, levels = lev[c(ref, seq(along = lev)[ - ref])], labels = lev[c(
       ref, seq(along = lev)[ - ref])])
}
, comment = "03/09/1999")
