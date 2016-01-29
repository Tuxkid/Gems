find.pos.mat <-
structure(function(mat = b, test = (b > 1))
{
# Finds positions in matrix of elements which meet test
    want <- mat[test]
    if(length(want) == 0)
       cat("No matches\n")
    else pass <- cbind(row(mat)[test], col(mat)[test])
    dimnames(pass) <- list(paste("Soln:", 1:dim(pass)[1], sep = ""), c("Row", 
       "Col"))
    pass
}
, comment = "13/05/1997")
