robust.deviance <-
structure(function(phat, n, r, min.expected = 1)
{
### phat : vector of fitted probabilities
### n : binomial totals
### r : binomial successes
### min.expected : groups combined so that minimum number expected 
    eps <- 1.0000000000000001e-15
    if(any(n < 0))
       stop("can't have 0 or negative binomial sample size")
    rhat <- phat * n
    ord <- order(phat)
    ind <- match(1:length(n), ord)
    cum.rhat <- cumsum(rhat[ord])
    g1 <- cum.rhat < min.expected
    g1 <- c(T, g1)[1:length(n)]
    revcum.rhat <- rev(cumsum(rev((n - rhat)[ord])))
    g3 <- revcum.rhat < min.expected
    g3 <- c(g3, T)[-1]
    g2 <- !(g1 | g3)
    df <- sum(g2)    ### Really, sum(g2)+2-2; the endpoints are always F
    grp1 <- rep(NA, length(n))
    grp1[g1] <- 1
    grp1[g2] <- 2:(sum(g2) + 1)
    grp1[g3] <- sum(g2) + 2
    grp <- grp1[ind]
    grp
    r.grp <- tapply(r, grp, sum)
    n.grp <- tapply(n, grp, sum)
    phat.grp <- tapply(phat * n, grp, sum)/tapply(n, grp, sum)
    qhat.grp <- 1 - phat.grp
    pobs <- r.grp/n.grp
    qobs <- 1 - pobs
    D1 <- ifelse(pobs < eps, log((pobs/phat.grp)^r.grp), r.grp * log(pobs/
       phat.grp))
    D2 <- ifelse(qobs < eps, log((qobs/qhat.grp)^(n.grp - r.grp)), (n.grp - 
       r.grp) * log(qobs/qhat.grp))
    dev <- 2 * sum(D1 + D2)
    list(dev = dev, df = df)
}
, comment = "26/06/1994")
