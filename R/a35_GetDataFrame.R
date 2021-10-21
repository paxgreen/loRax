


GetDataFrame <- function(numrow){
df0 <- mypredictors[numrow, ]
df1 <- df0$data %>%
  data.frame() %>%
  mutate(raster = df0$raster, id = df0$id)
return(df1)
}
