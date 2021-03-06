table.all <-
structure(function(x, own.seq = seq(0, 3, by = 0.5), minx = 0, maxx = NULL)
{
### returns a table that indicates a zero if the value is absent
### 1a) if else when specifying own sequence
    if(!is.null(own.seq)) {
       same.intervals <- mean(diff(own.seq))
       if(all(diff(own.seq) == same.intervals)) {
          table(c(x, own.seq)) - 1
       }
       else {
          stop("Sequence steps not all the same")
       }
    }
    else {
### 1b) when own sequence is NULL
### 2) if else when specifying a maximum for x
       if(!is.null(maxx)) {
          table(c(x, minx:maxx)) - 1
       }
       else {
          table(c(x, minx:max(x, na.rm = T))) - 1
       }
    }
}
, comment = "19/04/1997")
