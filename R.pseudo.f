  Listing of: pseudo.f
  Located in: /home/hrapgc/Rstuff/Gems
Last updated: 17/02/1998 
**************************************

"pseudo.f" <-
function(y, show.aov = F)
{
### Calculates pseudo F-statistics from an anova of a glm object y
    x <- anova(y)
    bits <- unlist(dimnames(x)[[1]])
    resid.df <- x[rev(bits)[1], "Resid. Df"]
    out.df <- NULL
    for(i in bits[-1]) {
       f.stat <- (x[i, "Deviance"]/x[i, "Df"])/(x[rev(bits)[1], "Resid. Dev"]/
          resid.df)
       prf <- 1 - round(pf(f.stat, x[i, "Df"], resid.df), 3)
       out.df <- rbind(out.df, data.frame(Factor = i, "F value" = round(f.stat,
          4), Df1 = x[i, "Df"], Df2 = resid.df, Prob = prf))
    }
    if(show.aov) {
       print(x)
       cat("\n")
    }
    out.df
}
