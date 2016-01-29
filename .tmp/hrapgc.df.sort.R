df.sort <-
structure(function(dfm, sort.by = names(dfm)[1], backwards = F)
{
### To sort the rows of a dataframe into an order set by vector "sort.by"
### If sort.by is numeric it is the column nos; if character, the column names.
  if(is.character(sort.by)) {
    sort.by.n <- match(sort.by, names(dfm))
    if(any(is.na(sort.by.n))) {
      stop(paste("\n", sort.by[is.na(sort.by.n)], 
                 "is not a recognised name"))
    }
    else sort.by <- sort.by.n
  }
  for(i in sort.by) {
    dfm.i <- dfm[order(dfm[, i]),  ]
    if(backwards)
      dfm.i <- dfm[(rev(order(dfm[, i]))),  ]
    dfm <- dfm.i
  }
  dfm
}
, comment = "13/11/2002")
