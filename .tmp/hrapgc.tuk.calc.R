tuk.calc <-
structure(function(comp = 3, error.df = 50, se = 0.2029, interval = 0.94999999999999996, 
    sed.logical = T)
{
    if(sed.logical) {
       qtukey(interval, comp, error.df) * (se/sqrt(2))
    }
    else {
       qtukey(interval, comp, error.df) * se
    }
}
, comment = "24/12/1996")
