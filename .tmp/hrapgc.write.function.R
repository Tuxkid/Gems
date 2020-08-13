write.function <-
function(x)
{
### writes a function to file in the ./scr/ directory
  fun.name <- as.character(substitute(x))
  filedate <- comment(get(fun.name))
  location <- system("pwd", TRUE)
    file.name <- ppaste(location,"/.src/R.", fun.name)
  write(paste("  Listing of:", fun.name), ncolumns = 1, file = file.name)
  write(paste("  Located in:", location), file = file.name, ncolumns = 1, append = TRUE)
  write(paste("Last updated:", filedate, 
              "\n**************************************\n"),
        file = file.name, append = TRUE)
  dump(fun.name, file = file.name, append = TRUE)
}

write.function <-
structure(function(x)
{
### writes a function to file in the ./scr/ directory
  fun.name <- as.character(substitute(x))
  filedate <- comment(get(fun.name))
  location <- system("pwd", TRUE)
    file.name <- ppaste(location,"/.src/R.", fun.name)
  write(paste("  Listing of:", fun.name), ncolumns = 1, file = file.name)
  write(paste("  Located in:", location), file = file.name, ncolumns = 1, append = TRUE)
  write(paste("Last updated:", filedate, 
              "\n**************************************\n"),
        file = file.name, append = TRUE)
  dump(fun.name, file = file.name, append = TRUE)
}
, comment = "22/08/2002")
