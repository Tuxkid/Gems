add.text <-
structure(function(xp = 0.5, yp = 0.5, tex = NULL, ygap = 0.05, cex = 0.8, adj = 0,
           adj.y = 0, font = 1, col = "black", srt = 0, ...)
{
### Cut down from bar.legs: used only for text; can also be rotated
### labels is character vector of legend text
### the length of tex sets the length of other vectors
### 
### 
### Establish positions in page co-ordinates [0,1] then use regular text function
### From Splus a/c began 11/07/1997
### Added vertical adj bit 17/6/04
  uu <- par()$usr
  xd <- diff(uu[1:2])
  yd <- diff(uu[3:4])
  yp <- yp - (seq(along = tex) - 1) * ygap
  if(length(xp) == 1)
    xp <- rep(xp, length(tex))
  if(length(cex) == 1)
    cex <- rep(cex, length(xp))
  if(length(adj) == 1)
    adj <- rep(adj, length(xp))
  if(length(font) == 1)
    font <- rep(font, length(xp))
  if(length(col) == 1)
    col <- rep(col, length(xp))
  xh <- uu[1] + xd * xp
  yh <- uu[3] + yd * yp
  for(i in 1:length(tex)) {
    text(xh[i], yh[i], tex[i], adj = c(adj[i], adj.y), cex = cex[i],
         font = font[i], col = col[i], ...)
  }
}
, comment = "23/04/2008")
