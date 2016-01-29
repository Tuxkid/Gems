tab.mat <-
structure(function(mat)
{
  rownames <- dimnames(mat)[[1]]
  colnames <- dimnames(mat)[[2]]
  cat(c("\t", paste(colnames, collapse = "\t")), "\n")
  for(i in 1:dim(mat)[1]) {
    cat(paste(c(rownames[i], mat[i,  ]), collapse = "\t"), "\n")
  }
}
, comment = "26/06/1995")
