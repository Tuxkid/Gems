print.char.matrix <-
structure(function (x, file = "", col.name.align = "cen", col.txt.align = "right", 
            cell.align = "cen", hsep = " | ", vsep = "-", csep = "-+-", row.names = TRUE, 
            col.names = FALSE, append = FALSE, top.border = TRUE, left.border = TRUE) 
{
### To print a data frame or matrix to a text file or screen
###   and having names line up with stacked cells
###
### First, add row names as first column (might be removed later)
  rownames <- dimnames(x)[[1]]
  x <- cbind(rownames, format(x)) # stops disappearing trailing zeros
  dimnames(x)[[1]] <- seq(nrow(x))
###  Set up some padding functions:
###

  pad.left <- function(z, pads) {
### Pads spaces to left of text
    padding <- paste(rep(" ", pads), collapse = "")
    paste(padding, z, sep = "")
  }
  pad.mid <- function(z, pads) {
### Centres text in available space
    padding.right <- paste(rep(" ", pads%/%2), collapse = "")
    padding.left <- paste(rep(" ", pads - pads%/%2), collapse = "")
    paste(padding.left, z, padding.right, sep = "")
  }
  pad.right <- function(z, pads) {
### Pads spaces to right of text
    padding <- paste(rep(" ", pads), collapse = "")
    paste(z, padding, sep = "")
  }
###  (Padding happens on the opposite side to alignment)
  pad.types <- c("left", "mid", "right")
  names(pad.types) <- c("right", "cen", "left")
  pad.name <- pad.types[col.name.align]
  pad.txt <- pad.types[col.txt.align]
  pad.cell <- pad.types[cell.align]
### Padding character columns
###    Need columns with uniform number of characters
  pad.char.col.right <- function(y) {
### For aligning text to LHS of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y)) out[i] <- pad.right(y[i], pads = padding[i])
    out
  }
  pad.char.col.left <- function(y) {
### For aligning text to RHS of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y)) out[i] <- pad.left(y[i], pads = padding[i])
    out
  }
  pad.char.col.mid <- function(y) {
### For aligning text to centre of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y)) out[i] <- pad.mid(y[i], pads = padding[i])
    out
  }
### which functions to use this time.
  pad.name.fn <- get(paste("pad.", pad.name, sep = ""))
  pad.txt.fn <- get(paste("pad.char.col.", pad.txt, sep = ""))
  pad.cell.fn <- get(paste("pad.", pad.cell, sep = ""))
###
### Remove troublesome factors
  x <- as.data.frame(x)
  fac.col <- names(x)[sapply(x, is.factor)]
  for (i in fac.col) x[, i] <- I(as.character(x[, i]))
### ARE ANY LINE BREAKS IN ANY COLUMNS?
  break.list <- list()
  for (i in seq(nrow(x))) {
    x.i <- unlist(x[i, ])
    rows.i <- sapply(strsplit(unlist(x[i, ]), "\n"), length)
    rows.i[rows.i < 1] <- 1
    break.list[[i]] <- rows.i
  }
  break.row <- sapply(break.list, function(x) any(x > 1))
  names(break.row) <- seq(nrow(x))
  xx <- x
  if (any(break.row)) {
### add in extra row/s
    xx <- NULL
    reprow <- lapply(break.list, unique)
    for (k in seq(nrow(x))) {
      x.k <- unlist(x[k, ])
      x.k[x.k == ""] <- " "
      if (break.row[k]) {
        l.k <- strsplit(x.k, "\n")
        add.blanks <- max(break.list[[k]]) - break.list[[k]]
        names(l.k) <- names(add.blanks) <- seq(length(l.k))
        if (any(add.blanks > 0)) {
          for (kk in names(add.blanks[add.blanks > 0]))
            l.k[[kk]] <- c(l.k[[kk]], rep(" ", add.blanks[kk]))
        }
        l.k.df <- as.data.frame(l.k)
        names(l.k.df) <- names(x)
        xx <- rbind(xx, as.matrix(l.k.df))
      }
      else xx <- rbind(xx, x.k)
   }
    row.names(xx) <- paste(rep(row.names(x), sapply(reprow, 
                                                    max)), unlist(reprow), sep = ".")
### Make an index for the rows to be printed
    rn <- row.names(xx)
    rnb <- strsplit(rn, "\\.")
    rpref <- codes(factor(sapply(rnb, function(z) z[1])))
  }
  else rpref <- seq(nrow(x))
  x <- as.data.frame(xx)
### Character columns need different treatment from numeric columns
  char.cols <- sapply(x, is.character)
  if (any(char.cols)) 
    x[char.cols] <- sapply(x[char.cols], pad.txt.fn)
### Change numeric columns into character
  if (any(!char.cols)) 
    x[!char.cols] <- sapply(x[!char.cols], format)
### now all character columns each of which is uniform element width
###
### Lining up names with their columns
### Sometimes the names of columns are wider than the columns they name, 
###  sometimes vice versa.
###
  names.width <- nchar(names(x))
  names.width[1] <- 1 # row.names will be removed, so names.width[1] != 8
  if (!col.names) 
    names.width <- rep(0, length(names.width))
  cell.width <- sapply(x, function(y) max(nchar(as.character(y))))
### (the width of the characters in the cells as distinct
###      from their names)  
  name.pads <- cell.width - names.width
  cell.pads <- -name.pads
  name.pads[name.pads < 0] <- 0
  cell.pads[cell.pads < 0] <- 0
  pad.names <- name.pads > 0
  pad.cells <- cell.pads > 0
### Pad out the column names if necessary:
  if (any(pad.names)) {
    stretch.names <- names(x)[pad.names]
    for (i in stretch.names) {
      names(x)[names(x) == i] <- pad.name.fn(i, name.pads[i])
    }
  }
### likewise for the cells and columns
  if (any(pad.cells)) {
    stretch.cells <- names(x)[pad.cells]
    for (j in stretch.cells) x[, j] <- pad.cell.fn(x[, j], 
                                                   cell.pads[j])
  }
### Remove row names if not required
  if (!row.names) 
    x <- x[-1]
### Put the column names on top of matrix
  if (col.names)  {
    mat2 <- rbind(names(x), as.matrix(x))
    if(row.names) # Don't remove first column name if there are no rownames
      mat2[1, 1] <- paste(rep(" ", cell.width[1]), collapse = "") # get rid of "rownames" from first column
  }
  else mat2 <- as.matrix(x)
  mat.names.width <- nchar(mat2[1, ])
### character string to separate rows
  space.h <- ""
  for (k in seq(mat.names.width)) {
    space.h <- c(space.h, rep(vsep, mat.names.width[k]), 
                 csep)
  }
  line.sep <- paste(c(ifelse(left.border, csep, ""), space.h), 
                    collapse = "")
### Maybe fix this up later...
  ##  line.sep <- substring(line.sep, 2, nchar(line.sep) -1) # remove first and last spaces
  if (col.names) 
    rpref <- c(0, rpref, 0)
  else rpref <- c(rpref, 0)
### print to screen or file

  if (top.border) {
    write(line.sep, file = file, append = append)
    append <- TRUE
  }
  for (i in 1:nrow(mat2)) {
    if (left.border) 
      write(paste(paste(c("", mat2[i, ]), collapse = hsep), 
                  hsep, sep = ""), file = file, append = append)
    else write(paste(paste(mat2[i, ], collapse = hsep), hsep, 
                     sep = ""), file = file, append = append)
    append <- TRUE
### print separator if row prefix is not same as next one
    if (rpref[i] != rpref[i + 1]) 
      write(line.sep, file = file, append = TRUE)
  }
}
, comment = "22/02/2007")
