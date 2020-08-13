mortDayEvent <-
structure(function(xx = trial4.df, trt.col = "Diet", rep.col = "Rep",
           tot.col = "Total", ded.col = "Dead")
{
### Purpose:- General form of getting mortality into event data
### ----------------------------------------------------------------------
### Modified from:- mortDayEventAphid in /home/hrapgc/Rstuff/bioprotection/joanne/
### ----------------------------------------------------------------------
### Arguments:- trt.col: name of column giving treatments
###             rep.col: name of column giving replicates
###             tot.col: name of column giving totals
###             ded.col: name of column giving number dead
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  9 Jul 2008, 10:49
### ----------------------------------------------------------------------
### Revisions:-

  ## Line up names with this lot of data
  xx$Treatment <- xx[, trt.col]
  xx$Rep <- xx[, rep.col]
  xx$Total <- xx[, tot.col]
  xx$Dead <- xx[, ded.col]
  ## should be now all named the same way
  xx$live <- with(xx, Total - Dead)
  xx <- unfactor(xx)
  xx$diff <- c(-10, diff(xx$Dead))
  use.df <- xx[xx$diff !=0,]
  use.df$diff[use.df$diff < 0] <- 0 # begin with no event
  out.df <- NULL
    for(T in unique(use.df$Treatment)){
      xx.T <- use.df[use.df$Treatment == T,]
      for(r in unique(xx.T$Rep)){
        df.id <- data.frame(Treatment = T)
        xx.Tr <- xx.T[xx.T$Rep == r,]
        events <- with(xx.Tr, rep(Days, diff))
        ## How much censoring happened?
        numleft <- rev(xx.Tr$live)[1]# How many left at end?
        cens <- c(rep(1, length(events)), rep(0, numleft))
        events <- c(events, rep(max(events), numleft))
        event.df.iTr <- df.id[rep(1, length(events)),, drop = FALSE]
        event.df.iTr$Day <- events
        event.df.iTr$cens <- cens
        out.df <- rbind(out.df, event.df.iTr)
     ##   browser()
      }
    }
  rownames(out.df) <- paste(seq(nrow(out.df)))
  out.df
}
, comment = "09/07/2008")
