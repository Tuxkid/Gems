df.to.file <-
structure(function(df = unfactor(junk.df), file = "", pad = "mid", append = F,
           row.nos = F, col.names = T)
{
### To print a data frame to a text file and having names line up with values
###    underneath, and no need to use tab spacing
###
###  Set up some padding functions:  Need columns with same number of characters
###    all the way down.
  pad.char.cols <- function(y)
    {
                                        # For padding character columns
      biggest <- max(nchar(y))
      col.width <- nchar(y)
      padding <- paste(rep(" ", biggest - min(col.width)), collapse = "")
      out <- substring(paste(y, padding, sep = ""), 1, biggest)
      out
    }
  pad.left <- function(z, pads)
    {
      padding <- paste(rep(" ", pads), collapse = "")
      paste(padding, z, sep = "")
    }
  pad.mid <- function(z, pads)
    {
                                        # Centres text in available space
      padding.right <- paste(rep(" ", pads %/% 2), collapse = "")
      padding.left <- paste(rep(" ", pads - pads %/% 2), collapse = "")
      paste(padding.left, z, padding.right, sep = "")
    }
  pad.right <- function(z, pads)
    {
      padding <- paste(rep(" ", pads), collapse = "")
      paste(z, padding, sep = "")
    }
### Character columns need different treatment from numeric columns
#browser()
  char.cols <- sapply(df, is.character)
  if(any(char.cols))
  df[char.cols] <- sapply(df[char.cols], pad.char.cols)
  if(any(!char.cols))
  df[!char.cols] <- sapply(df[!char.cols], format)    #
### Sometimes the names of columns are wider than the columns they name, 
###  sometimes vice versa.
  names.width <- nchar(names(df))
  row.width <- sapply(df, function(x)
                      max(nchar(x)))    #
#browser()
### (the width of the characters in the columns (and rows) as distinct
###      from their names
  name.pads <- row.width - names.width
  row.pads <- name.pads * -1
  name.pads[name.pads < 0] <- 0
  row.pads[row.pads < 0] <- 0
  pad.names <- name.pads > 0
  pad.rows <- row.pads > 0    #
### Pad out the column names if necessary:
  pad.funct <- get(paste("pad.", pad, sep = ""))
  if(any(pad.names)) {
    stretch.names <- names(df)[pad.names]
    for(i in stretch.names) {
      names(df)[names(df) == i] <- pad.funct(i, name.pads[i])
    }
  }
### likewise for the rows and columns
  if(any(pad.rows)) {
    stretch.rows <- names(df)[pad.rows]
    for(j in stretch.rows)
      df[, j] <- pad.funct(df[, j], row.pads[j])
  }
### the write() function doesn't use the names of the matrix, so put them
###   into it. 
  df2 <- df
  if(row.nos) {
    df2 <- cbind(seq(nrow(df)), df)
    no.wid <- nchar(nrow(df))
    no.col.name <- paste(rep(" ", no.wid), collapse = "")
    names(df2) <- c(no.col.name, names(df))
  }
  if(col.names)
    df2 <- rbind(names(df2), as.matrix(df2))
  else df2 <- as.matrix(df2)
  write(t(df2), file, ncol(df2), append = append)
}
, comment = "24/07/2001")
