plot.resid <-
structure(function(xx)
{
  ## Purpose: Plots fitted values against the residuals for aovs, lms, etc
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: xx is an aov, glm, lm type object 
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 26 Aug 2005, 16:14

with(xx, plot(fitted.values, residuals))

}
, comment = "26/08/2005")
