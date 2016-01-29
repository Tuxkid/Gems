link.function <-
structure(function(link)
{
  link.options <- c("identity", "log", "logsurv", "logit", "sqrt", "inverse",
                    "probit", "loglog", "cloglog", "help")
  if(link == "help")
    cat("\nOptions are:", paste(link.options, sep = "  "), "\n")
  link.int <- charmatch(link, link.options)
  if(is.na(link.int))
    stop("Invalid link type")
  else if(link.int == 0)
    stop("Ambiguous link type")
  g <- switch(link,
              identity = function(p)
              p,
              log = function(p)
              log(p),
              logsurv = function(p)
              - log(1 - p),
              logit = function(p)
              {
                p <- ifelse(p > 1, NA, ifelse(p < 0, NA, p))
                p1 <- ifelse(p > 1 - 9.9999999999999998e-17, 1 - 
                             9.9999999999999998e-17, ifelse(p < 1.0000000000000001e-16, 
                                                            1.0000000000000001e-16, p))
                log(p1/(1 - p1))
              }
              ,
              sqrt = function(p)
              sqrt(p),
              inverse = function(p)
              1/p,
              probit = qnorm,
              loglog = ,
              cloglog = function(p)
              {
                p <- ifelse(p > 1, NA, ifelse(p < 0, NA, p))
                p1 <- ifelse(p > 1 - 9.9999999999999998e-8, 1 - 
                             9.9999999999999998e-8, ifelse(p < 1.0000000000000001e-8, 
                                                            1.0000000000000001e-8, p))
                log((log(1 - p1)*-1))
              }
              )
  
  g
}
, comment = "16/09/2002")
