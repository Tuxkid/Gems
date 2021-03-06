Text.rpart <-
structure(function(x, splits = TRUE, label = "yval", FUN = text, all=FALSE,
           pretty = NULL, digits = getOption("digits") - 3,
           use.n=FALSE, fancy=FALSE, fwidth=.8, fheight =.8,
           leaf.col = "darkorange",...)
{
  ## Purpose: Modifies text.rpart to position text better
  ## ----------------------------------------------------------------------
  ## Modified from: text.rpart in the basic distribution
  ## ----------------------------------------------------------------------
  ## Arguments: 
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 21 Jan 2005, 16:39
  if(!inherits(x, "rpart")) stop("Not legitimate rpart")
  if(!is.null(x$frame$splits)) x <- rpconvert(x)#Backwards compatability

  frame <- x$frame
  col <- names(frame)
  ylevels <- attr(x,'ylevels')
  if(!is.null(ylevels <- attr(x, "ylevels")))
    col <- c(col, ylevels)
  if(is.na(match(label, col)))
    stop("Label must be a column label of the frame component of the tree"
         )
  cxy <- par("cxy")                   #character width and height
  if(!is.null(srt <- list(...)$srt) && srt == 90)
    cxy <- rev(cxy)
  xy <- rpart:::rpartco(x)

  node <- as.numeric(row.names(x$frame))
  is.left <- (node%%2 ==0)            #left hand sons
  node.left <- node[is.left]
  parent <- match(node.left/2, node)

  ##Put left splits at the parent node

  if(splits) {
    left.child <- match(2 * node, node)
    right.child <- match(node * 2 + 1, node)
    rows <- labels(x, pretty = pretty)

    if(fancy) {
      ## put split labels on branches instead of nodes

      xytmp <- rpart:::rpart.branch(x=xy$x,y=xy$y,node=node)
      leftptx <- (xytmp$x[2,]+xytmp$x[1,])/2
      leftpty <- (xytmp$y[2,]+xytmp$y[1,])/2
      rightptx <- (xytmp$x[3,]+xytmp$x[4,])/2
      rightpty <- (xytmp$y[3,]+xytmp$y[4,])/2

      FUN(leftptx,leftpty+.52*cxy[2],
          rows[left.child[!is.na(left.child)]],...)
      FUN(rightptx,rightpty-.52*cxy[2],
          rows[right.child[!is.na(right.child)]],...)
    }

    else FUN(xy$x, xy$y + 0.5 * cxy[2], rows[left.child], ...)
  }
  leaves <- if(all) rep(TRUE, nrow(frame)) else frame$var == "<leaf>"
  if (is.null(frame$yval2))
    stat <- x$functions$text(yval=frame$yval[leaves],
                             dev=frame$dev[leaves],
                             wt=frame$wt[leaves],
                             ylevel=ylevels, digits=digits,
                             n=frame$n[leaves], use.n=use.n)
  else
    stat <- x$functions$text(yval=frame$yval2[leaves,],
                             dev=frame$dev[leaves],
                             wt=frame$wt[leaves],
                             ylevel=ylevels, digits=digits,
                             n=frame$n[leaves], use.n=use.n)


  oval <- function(middlex,middley,a,b) {

    theta <- seq(0,2*pi,pi/30)
    newx <- middlex + a*cos(theta)
    newy <- middley + b*sin(theta)

    polygon(newx,newy,border=TRUE,col=0)
    ##	     polygon(newx,newy,border=T)
  }

  rectangle <- function(middlex, middley,a,b) {

    newx <- middlex + c(a,a,-a,-a)
    newy <- middley + c(b,-b,-b,b)

    polygon(newx,newy,border=TRUE,col=0)
    ##	  polygon(newx,newy,border=T)
  }

  if(fancy) {

    ## find maximum length of stat
    maxlen <- max(rpart:::string.bounding.box(stat)$columns) + 1
    maxht <- max(rpart:::string.bounding.box(stat)$rows) +1

    if(fwidth<1)  a.length <- fwidth*cxy[1]*maxlen
    else a.length <- fwidth*cxy[1]

    if(fheight<1) b.length <- fheight*cxy[2]*maxht
    else b.length <- fheight*cxy[2]

### create ovals and rectangles here
    ## sqrt(2) creates the smallest oval that fits around the
    ## best fitting rectangle
    for(i in parent) oval(xy$x[i],xy$y[i],
                          a=sqrt(2)*a.length/2, b=sqrt(2)*b.length/2)
    child <- match(node[frame$var=="<leaf>"],node)
    for(i in child) rectangle(xy$x[i],xy$y[i],
                              a=a.length/2,b=b.length/2)
  }

  ##if FUN=text then adj=1 puts the split label to the left of the
  ##    split rather than centered
  ##Allow labels at all or just leaf nodes

  ## stick values on nodes
  if(fancy) FUN(xy$x[leaves], xy$y[leaves] + .5 * cxy[2], stat, ...)
  else FUN(xy$x[leaves], xy$y[leaves] -0.9 * cxy[2], stat, adj=.5,
           col = leaf.col, ...)

  invisible()

}
, comment = "07/07/2008")
