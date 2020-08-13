levels.for.genstat <-
structure(function(x, no.quotes = T)
{
# Returns a string of the factor levels to use in genstat
# paste from between " marks
# change the opening and closing "   to ' if quotes are wanted
  paste(unique(x), collapse = ifelse(no.quotes, ", ", "', '"))
}
, comment = "27/03/2003")
