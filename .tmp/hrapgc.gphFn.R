gphFn <-
structure(function(myVC, myData, myGNames = c('genotype','year'),
           phenCor = 'raw', ignoreXns = TRUE)
{
  VCNames <- row.names(myVC)
  ##  Define lines containing genotype (GLines), residual (RLines)
  ##  and interaction (XLines - later) var/cov
  GLines <- grep(ppaste('trait:', myGNames[1], '!'), VCNames)
  RLines <- grep('R!trait', VCNames)
  ## get Names of variates
  myNames <- unique(sapply(strsplit(VCNames[RLines], ":"), "[", 2))
  myN <- length(myNames)  ## myN is the number of variates
  myCse <- myRse <- myR <- myGse <- myG <-
    matrix(nrow = myN, ncol = myN, dimnames = list(myNames, myNames))
  myG[upper.tri(myG, diag = TRUE)] <- myVC[GLines, "component"]
  myGse[upper.tri(myG, diag = TRUE)] <- myVC[GLines, "std.error"]
  myR[upper.tri(myR, diag = TRUE)] <- myVC[RLines,"component" ]
  myRse[upper.tri(myR, diag = TRUE)] <- myVC[RLines, "std.error"]
  myGR <- myG + myR
  myGRse <- sqrt(myGse^2 + myRse^2)
  if(!ignoreXns) {## accommodate Xns following Fry (1992)
    XLines <- matrix(NA, length(GLines), length(myGNames) -1)
    for (myi in 2:length(myGNames)) XLines[, myi -1] <-
      regexpr(myGNames[1], VCNames) > 0 & regexpr(myGNames[myi],
                                                VCNames) > 0
## AA     regexpr(paste(myGNames[1], myGNames[myi], sep = ':'), VCNames) > 0
    myX <- myXse <- myG
    myNLev <- sapply(as.data.frame(myData[, myGNames[-1]]), nlevels)
    myX[upper.tri(myX, diag = T)] <- myVC[XLines[, 1],2]
    myXse[upper.tri(myX, diag = T)] <- myVC[XLines[, 1],3]
    myG <- myG+myX/myNLev[1]
    myGR <- myGR+myX
    myGse <- sqrt(myG^2 + (myX^2)/myNLev)
    myGRse <- sqrt(myGRse^2+myX^2)
  }
  myGR <- t(myGR)
  myGRse <- t(myGRse)
  myC <- cov2cor(myG)
  diag(myC) <- diag(myG)/diag(myGR)
  for (myi in 1:(myN - 1)) {
    for(myj in (myi + 1):myN) {
      myCse[myi,myj] <- sqrt(myGse[myi, myj]^2/
                             (myG[myi, myi] * myG[myj, myj]) +
                             (myGse[myi, myi] * myG[myi, myj])^2/
                             (4 * myG[myi, myi]^3 * myG[myj, myj]) +
                             (myGse[myj, myj] * myG[myi, myj])^2/
                             (4*myG[myi, myi] * myG[myj, myj]^3))
    }
  }
  if (tolower(substring(phenCor, 1, 1)) == 'r') {
    myC[lower.tri(myC)] <- cor(myData[, myNames],
                               use = 'pair')[lower.tri(
                                 cor(myData[, myNames], use = 'pair'))]
    for (myj in 1:(myN - 1)){
      for (myi in (myj + 1):myN){
        myEstTemp <- cor.test(myData[, myNames[myi]],
                              myData[, myNames[myj]], use = 'pair')
        myCse[myi, myj] <- myEstTemp$est/myEstTemp$stat
      }
    }
  }
  else  {
    myC[lower.tri(myC)] <- cov2cor(myGR)[lower.tri(myGR)]
    for (myj in 1:(myN - 1)) {
      for (myi in (myj + 1):myN) {
        myCse[myi, myj] <- sqrt(myGRse[myi, myj]^2/
                                (myGR[myi, myi] * myGR[myj,myj]) +
                                (myGRse[myi, myi] * myGR[myi, myj])^2/
                                (4 * myGR[myi, myi]^3 * myGR[myj, myj]) +
                                (myGRse[myj, myj] * myGR[myi, myj])^2/
                                (4 * myGR[myi, myi] * myGR[myj, myj]^3))
      }
    }
  }
  diag(myCse) <- sqrt((diag(myGse) * diag(myR))^2 +
                      (diag(myG) * diag(myRse))^2)/(diag(myGR)^2)
##  attr(myC, 'creation time') <- attr(myCse, 'creation time') <- Sys.time()
  list(correlations = myC, stdErrors = myCse)
}
, comment = "11/03/2008")
