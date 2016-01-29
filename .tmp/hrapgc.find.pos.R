find.pos <-
structure(function(y, inn)
{
#
# Finds positions of "y" in a vector "inn"
    x <- match(inn, y)
    seq(along = x)[!is.na(x)]
}
, comment = "30/04/2001")
