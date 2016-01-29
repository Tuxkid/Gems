wrap.text <-
structure(function(tex = "", wid = 30)
{
  ## Purpose: Wraps text at wid characters wide (or smaller if necessary)
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##       tex: character string
  ##       wid: how wide before the text is wrapped
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 19 Mar 2007, 14:27
  out.vec <- NULL
  tex.leng <- nchar(tex)
  tex.vec <-  c(sapply(strsplit(tex, ""), paste, sep = ""))
  while(tex.leng > wid){
    tex.leng <- length(tex.vec)
    spaces <- which(tex.vec == " ")
    need.break <- spaces > wid
    if(any(need.break) > 0){
      break.at <- spaces[which(need.break)[1] -1]
      out.vec <- c(out.vec, "\n", tex.vec[1:break.at -1])
      tex.vec <- tex.vec[(break.at + 1):tex.leng]
    } else {
### one last break seems to be necessary now
      if(length(tex.vec) > wid){
        last.break <- rev(spaces)[1]
        tex.vec[last.break] <- "\n"
      }
      out.vec <- c(out.vec, "\n", tex.vec)
      tex.vec <- ""
    }
  }
  paste(out.vec[-1], collapse = "")
}
, comment = "19/03/2007")
