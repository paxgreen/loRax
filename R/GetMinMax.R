GetMinMax <- function(mydata){
  dato <- mydata[[3]][[2]]$transactions$transaction
  minmax <<- dato$minmax
  return(minmax)
}
