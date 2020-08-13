set.screens <-
structure(function(across = 2, down = 3, left.margin = 10, top.margin = 20, height = 80, 
    width = 90, ygap = -10, xgap = -1, overwrite = T, centre.x = T, centre.y = 
    T, byrow = T)
{
# Uses split.screen for setting up screens: alternative to par(mfrow/mfcol)
    uu <- ps.region/72 * 25.4    # Change points to mm
    x.all <- uu[3] - uu[1]
    y.all <- uu[4] - uu[2]    #
# Check if there is room for what has been set
    spare.x <- x.all - across * width - xgap * (across - 1)
    if(spare.x < 0)
       stop("\nInsufficient space:\nCheck left.margin, width and xgap")
    spare.y <- y.all - down * height - ygap * (down - 1)
    if(spare.y < 0) stop(
          "\nInsufficient space:\nCheck top.margin, height and ygap")    #
# Set up coordinates for the screens
    top <- top.margin + ifelse(centre.y, spare.y/(down + 1), 0)
    left <- left.margin + ifelse(centre.x, spare.x/(across + 1), 0)
    N <- across * down
    first.x.corners <- c(left - uu[1], left - uu[1] + width)/x.all
    first.y.corners <- c(uu[4] - top - height, uu[4] - top)/y.all    #browser()
    if(N == 1)
       pos.mat <- matrix(c(first.x.corner, first.y.corner), nr = 1)
    else {
       pos.mat <- NULL
       if(byrow) {
          for(j in 1:down) {
             y.corners.j <- first.y.corners - (ygap + height)/y.all * (j - 1)
             for(i in 1:across) {
                x.corners.i <- first.x.corners + ((width + xgap) * (i - 1))/
                   x.all
                pos.mat <- rbind(pos.mat, c(x.corners.i, y.corners.j))
             }
          }
       }
       else {
          for(i in 1:across) {
             x.corners.i <- first.x.corners + ((width + xgap) * (i - 1))/x.all
             for(j in 1:down) {
                y.corners.j <- first.y.corners - (ygap + height)/y.all * (j - 1
                   )
                pos.mat <- rbind(pos.mat, c(x.corners.i, y.corners.j))
             }
          }
       }
       split.screen(pos.mat, erase = !overwrite)
    }
}
, comment = "29/11/1997")
