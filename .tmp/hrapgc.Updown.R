Updown <-
structure(function(cv)
{
  ## Bill Venables's function (adjusted) to make uppercase first
  ##  letters, and lower the rest.
  cv <- casefold(cv)
  first <- substring(cv, 1, 1)
  FIRST <- casefold(first, upper = T)
  paste(FIRST, substring(cv, 2), sep = "")
}
, comment = "05/04/2002")
