formula.variables <-
structure(function(myformula, unwanted = NULL)
{
    ops <- c("~", "+", "-", "*", "|", "I", "(", ")", "sqrt", "log", "exp", 
       "logit", "cloglog", "ang", unwanted)
    out <- unlist(myformula)
    out[!(out %in% ops)]
}
, comment = "10/10/1997")
