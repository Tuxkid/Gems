errorbars <-
structure(function(x, y, se, yl = y - se, yu = y + se, eps, ...)
{
    if(any(is.na(yl)) | any(is.na(yu)))
       text(x[is.na(yl) | is.na(yu)], y[is.na(yl) | is.na(yu)], "NA", adj = 1)
    segments(x, yl, x, yu, ...)
    segments(x - eps, yl, x + eps, yl, ...)
    segments(x - eps, yu, x + eps, yu, ...)
}
, comment = "31/06/1996")
