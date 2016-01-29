remove.shading <-
structure(function()
{
# To get rid of shading on shingles above plots
    strip.background <- trellis.par.get("strip.background")
    strip.background$col <- 0
    trellis.par.set("strip.background", strip.background)
    strip.shingle <- trellis.par.get("strip.shingle")
    strip.shingle$col <- 0
    trellis.par.set("strip.shingle", strip.shingle)
}
, comment = "09/07/2001")
