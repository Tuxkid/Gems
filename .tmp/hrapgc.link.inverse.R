link.inverse <-
structure(function(link = "help")
{
    link.options <- c("identity", "log", "logsurv", "logit", "sqrt", "inverse",
       "probit", "loglog", "cloglog", "help")
    if(link == "help")
       cat("\nOptions are:", paste(link.options, sep = "  "), "\n")
    link.int <- charmatch(link, link.options)
    if(is.na(link.int))
       stop(paste("Invalid link type: \nOptions are:", paste(link.options, 
          collapse = " ")))
    else if(link.int == 0)
       stop("Ambiguous link type")
    f <- switch(link,
       identity = function(x)
       x,
       log = function(x)
       exp(x),
       logsurv = function(x)
       1 - exp( - x),
       logit = function(x)
       {
          z <- exp(ifelse(x > 80, 80, ifelse(x < -80, -80, x)))
          z/(z + 1)
       }
       ,
       sqrt = function(x)
       x^2,
       inverse = function(x)
       1/x,
       probit = pnorm,
       loglog = ,
       cloglog = function(x)
       {
          z <- exp(ifelse(x > 80, 80, ifelse(x < -80, -80, x)))
          1 - exp(.Uminus(z))
       }
       )
    f
}
, comment = "16/10/1994")
