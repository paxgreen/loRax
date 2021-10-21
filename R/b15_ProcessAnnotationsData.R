
ProcessAnnotationsData <- function(mydata){

  mutate <- dplyr::mutate
  row_number <- dplyr::row_number

  dato <- mydata[[3]][[2]]$transactions$transaction

  meta <<- dato$metadata
  annotationrasters <<- dato$rasters
  a0 <- dato$annotations

  edf <- data.frame()
  for (k in 1:nrow(a0)){
    df0 <- a0[k,]$data %>% data.frame()
    edf <- rbind(edf, df0)
  }

  annotations <<- edf %>%
    mutate(rownum  = row_number())

}
