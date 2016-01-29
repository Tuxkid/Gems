get.formula <-
structure(function(ab.list = ab.second.comb, bit = "cs", link = "cloglog", rnd = 4)
{
# Gets the formula for lines from ab list
    ab <- ab.list[[paste(bit)]]
    legends <- ab$legend
    glean <- get(ab$datafun)
    cat(glean()$maint)
    cat("\n\n")
    out.list <- list()
    for(i in 1:len(legends))
       out.list[[ab$legend[i]]] <- ppaste(link, "(p) = ", round(ab$intercept[i],
          rnd), " + ", round(ab$slope[i], rnd), " * t")
    out.list
}
, comment = "17/04/1999")
