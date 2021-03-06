get.mort.tables <-
structure(function(ab.list = ab.second.comb, bit = "cs", intervals = NULL, rnd = 4, file
     = "")
{
# Does table of expected mortality for lines from ab list
    ab <- ab.list[[paste(bit)]]
    legends <- ab$legend
    glean <- get(ab$datafun)
    cat(glean(choice = bit)$maint)    # browser()
    cat("\n\n")
    if(is.null(intervals))
       intervals <- seq(0, round(max(glean(choice = bit)$time)))
    out.list <- list(times = intervals)
    for(i in 1:len(legends))
       out.list[[ab$legend[i]]] <- round(cloglog.bt(ab$intercept[i] + ab$slope[
          i] * intervals) * 100, 1)
    out.df <- as.data.frame(out.list)
    if(file != "") {
       write(glean(choice = bit)$maint, file = file)
       df.to.file(out.df, file = file, append = T)
    }
    else out.df
}
, comment = "13/05/1999")
