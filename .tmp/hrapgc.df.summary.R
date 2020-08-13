df.summary <-
structure(function(data, y.cols = NULL, split.cols = NULL, rep.col = "Rep",
           decimals = 3, stats = "mean", help = F, ...)
{
### cut down and modified from summarize()
### Keep Stephen's beginning:
### Requires text.bp() and df.to.file() 
###   [anything else will be in   /home/hrapgc/Gems/.Data]
  summary.names <- c("N", "Missing", "Mean", "Median", "Trimmed Mean", 
                     "Std. Dev.", "SEM", "Min", "Max", "1st Quartile",
                     "3rd Quartile", "Sum", "Boxplots")
  summary.types <- c("num", "missing", "mean", "median", "trm", "stdev", 
                     "sem", "min", "max", "q1", "q3", "sum", "boxplots")
  help.df <- (data.frame("Select one of these" = summary.types, "to get this"
                         = summary.names))
  if(help) {
    cat("\n In the argument \"stats\"\n")
    print(help.df)
    return()
  }
### A few error messages:
  if(length(stats) > 1)
    stop(
         "\n\tOnly one statistic can be calculated at a time:\n\tIf you require multiple stats with only one y.cols, \n\tyou can use summarize() with df.out set to TRUE"
         )

### Make special functions to use in apply calls
  q1 <- function(x)
    quantile(x, na.rm = T)[2]
  q3 <- function(x)
    quantile(x, na.rm = T)[4]
  no.missing <- function(x)
    length(x[is.na(x)])
  round.mean <- function(x, y = decimals)
    round(mean(x, na.rm = T), y)
  round.sem <- function(x, y = decimals)
    round(sem(x, na.rm = T), y)
  round.sum <- function(x, y = decimals)
    round(sum(x, na.rm = T), y)
  add.boxplot <- function(x, y.range = range.j, width = bp.width)
    text.bp(x, y.range, width)    #
### Functions are now set up:
###
### Make named vector of functions to be used:
  use.functions <- c("length", "no.missing", "round.mean", "median", "trm", 
                     "stdev", "round.sem", "min", "max", "q1", "q3",
                     "round.sum", "add.boxplot")
  names(use.functions) <- summary.types
  names(summary.names) <- summary.types    #
### Use dataframes instead of matrices:
  if(is.matrix(data))
    data <- unfactor(as.data.frame(data))
  else data <- unfactor(data)    # avoid pesky factors
  data <- df.sort(data, rev(split.cols))    #
### Make character vectors of y.cols and split.cols if necessary:
  if(is.numeric(y.cols))
    y.cols <- names(data)[y.cols]
  if(is.numeric(split.cols)) split.cols <- names(data)[split.cols]    #
### Create index and adjust for single split columns if necessary:
  indx.df <- data.frame(data[, split.cols])
  names(indx.df) <- split.cols    # Necessary for single split.cols
  attach(indx.df)
  indx <- NULL
  for(i in split.cols)
    indx <- paste(indx, get(i), sep = ":")
  detach("indx.df")
  out.df <- data.frame(indx.df[match(unique(indx), indx), split.cols])
  names(out.df) <- split.cols    # Necessary for single split.cols
  dimnames(out.df) <- list(paste(1:nrow(out.df)), names(out.df))
## If stats is a function itself, use it: otherwise, use the list of functions
  if(class(stats) == "function"){
    for(j in y.cols)
      out.df[, j] <- s.tapply(data[, j], indx, stats, ...)
  }  else
  for(j in y.cols)
    out.df[, j] <- s.tapply(data[, j], indx, get(use.functions[stats]))
  out.df
}
, comment = "22/12/2006")
