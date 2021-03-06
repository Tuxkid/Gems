summarize <-
structure(function(data, y.cols = NULL, split.cols = NULL, rep.col = "Rep", file = "", 
           append = FALSE, help = FALSE, bp.width = 30, between = FALSE, dec = 3,
           select = c("num", "mean", "sem", "min", "max", "boxplots"), 
           df.out = FALSE, pad = "mid", show.type = FALSE)
{
### Keep Stephen's beginning:
### Requires text.bp(), df.summary(), and df.to.file()
  summary.names <- c("N", "Missing", "Mean", "Median", "Trimmed Mean", 
                     "Std. Dev.", "SEM", "Min", "Max", "1st Quartile",
                     "3rd Quartile", "Sum", "Boxplots")
  summary.types <- c("num", "missing", "mean", "median", "trm", "stdev", 
                     "sem", "min", "max", "q1", "q3", "sum", "boxplots")
  help.df <- (data.frame(Select.this = summary.types, to.get.this = 
                         summary.names))
  if(help) {
    cat("\n In the argument \"select\"\n")
    print(help.df)
    return()
  }

### Make character vectors of y.cols and split.cols if necessary:
  if(is.numeric(y.cols))
    y.cols <- names(data)[y.cols]
  if(is.numeric(split.cols)) split.cols <- names(data)[split.cols]    #
### If between reps are needed, first find means for each rep
  if(between & show.type) {
    if(file == "")
      cat("\n\t~~~~~~~~~~~ BETWEEN REPLICATES ~~~~~~~~~~~\n\n")
    else write("\n\t~~~~~~~~~~~ BETWEEN REPLICATES ~~~~~~~~~~~\n\n", file = 
               file, append = append)
    append <- TRUE
    data <- df.summary(data = data, y.cols = y.cols,
                       split.cols = c(split.cols, rep.col), decimals = 6)
  }
  else {
    if(show.type){
      if(file == "")
        cat("\n\t********* WITHIN REPLICATES *********\n\n")
      else write("\n\t********* WITHIN REPLICATES *********\n\n", file = file,
                 append = append)
      append <- TRUE
    }
  }
  s.tapply <- function(x, y, z, ...)
    tapply(x, y, z, ...)[(unique(y))]
  q1 <- function(x)
    quantile(x, na.rm = TRUE)[2]
  q3 <- function(x)
    quantile(x, na.rm = TRUE)[4]
  no.missing <- function(x)
    length(x[is.na(x)])
  len.rm <- function(x)
    length(x[!is.na(x)])
  round.stdev <- function(x, y = dec)
    round(stdev(x, na.rm = TRUE), y)
  round.mean <- function(x, y = dec)
    round(mean(x, na.rm = TRUE), y)
  round.sem <- function(x, y = dec)
    round(sem(x, na.rm = TRUE), y)
  round.max <- function(x, y = dec)
    round(max(x, na.rm = TRUE), y)
  round.min <- function(x, y = dec)
    round(min(x, na.rm = TRUE), y)
  round.median <- function(x, y = dec)
    round(median(x, na.rm = TRUE), y)
  add.boxplot <- function(x, y.range = range.j, width = bp.width)
    text.bp(x, y.range, width)    #
### Functions are now set up:
                                        #
### Make named vector of functions to be used:
  use.functions <- c("len.rm", "no.missing", "round.mean", "round.median", 
                     "trm", "round.stdev", "round.sem", "round.min", "round.max",
                     "q1", "q3", "sum", "add.boxplot")
  names(use.functions) <- summary.types
  names(summary.names) <- summary.types    #
### Use dataframes instead of matrices:
  if(is.matrix(data))
    data <- unfactor(as.data.frame(data))
  else data <- unfactor(data)    ### avoid pesky factors
  data <- df.sort(data, rev(split.cols))    #
### Create index and adjust for single split columns if necessary:
  indx.df <- data.frame(data[, split.cols])
  names(indx.df) <- split.cols    ### Necessary for single split.cols
  attach(indx.df)
  indx <- NULL
  for(i in split.cols)
    indx <- paste(indx, get(i), sep = ":")
  detach("indx.df")
  out.df <- data.frame(indx.df[match(unique(indx), indx), split.cols])
  names(out.df) <- split.cols    ### Necessary for single split.cols
  dimnames(out.df) <- list(paste(1:nrow(out.df)), names(out.df))
  for(j in y.cols) {
    range.j <- range(data[, j], na.rm = TRUE)
    for(k in select)
      out.df[[summary.names[k]]] <- as.vector(s.tapply(data[, j], indx,
                                                       get(use.functions[k])))
    if(df.out) {
      names(out.df) <- c(split.cols, paste(j, select, sep = "."))
      return(out.df)
    }
    else {
### Get rid of weird row number names:
      dimnames(out.df)[[1]] <- seq(nrow(out.df))
      out.df <- unfactor(out.df)
      numeric.cols <- !sapply(out.df, function(x)
                              any(is.na(as.numeric(x))))
      ## add in as.vector to cope with R-1.8.1 26/11/03 (and above)
      out.df[numeric.cols] <- as.vector(lapply(out.df[numeric.cols], as.numeric))
      underlines <- paste(rep("=", nchar(j)), collapse = "")
      if(file == "") {
        cat(paste("\n ", j, "\n", sep = ""))
        cat(paste(" ", underlines, "\n", sep = ""))
        print(out.df)
      }
      else {
        write(paste("\n", j), file = file, append = append)
        append <- TRUE    # hereafter appending will always be wanted
        write(paste(" ", underlines, sep = ""), file = file, append = TRUE)
        df.to.file(out.df, file = file, append = append, pad = pad)
      }
    }
  }
  invisible()
}
, comment = "23/12/2009")
