mean.lt.short <-
structure(function(lt.vector, lt = 99, intv = 95)
{
                                        # Shortened version of mean.lt()
  my.var <- function(x) var(x, na.rm = T)
  interval <- intv/100
  lts <- round(as.numeric(lt.vector), 5)
  lts <- lts[!is.na(lts)]
  reps <- length(lts)    # Change vector into single column matrix
  lt.3 <- matrix(lts, nc = 1)    #
                                        # Have vector of grouped lts. Continue finding means, etc.
  av.log.lt <- mean(log(lts), na.rm = T)
  var.log.lt <- my.var(log(lts))    #
                                        # Replace any missing variances with zero 
  sem.log.lt <- sem(log(lts), na.rm = T)
  deg <- length(lts) - 1
  delta <- qt(1 - (1 - interval)/2, deg) * sqrt(var.log.lt/reps)
  log.up <- av.log.lt + delta
  log.low <- av.log.lt - delta
  lt.mean <- round(exp(av.log.lt), 1)
  upper <- round(exp(log.up), 1)
  lower <- round(exp(log.low), 1)
  sem <- round(sem.log.lt * lt.mean, 3)    #
                                        # Make vector of means, etc
  y <- matrix(c(reps, lt.mean, lower, upper, sem), nr = 1)
  dimnames(y) <- list(NULL, c("reps", "lt.mean", "lower", "upper", " sem"))
                                        # Put individual LTs into vector
  mm <- matrix(round(lts, 1), nr = 1)
  dimnames(mm) <- list(NULL, paste("Rep", 1:length(mm), sep = ""))    
                                        # Output to screen
  cat(c(paste("Separate LT", lt, "s ", sep = ""), "\n\n"))
  tab.mat(mm)
  cat("\n\n")
  cat(c(paste("Mean LT", lt, "s and ", intv, "% c.i.:", sep = ""), "\n\n"))
                                        #if(dim(y)[1]==1)
                                        #x_matrix(x,nc=5,dimnames=dimnames(y))
  tab.mat(y)
  cat("\n\n")
}
, comment = "20/08/1996")
