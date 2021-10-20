
CreateSimulation <- function(numrow){

  predict <- stats::predict
  rename <- dplyr::rename
  bind_cols <- dplyr::bind_cols
  rename <- dplyr::rename
  mutate <- dplyr::mutate
  select <- dplyr::select
  group_by <- dplyr::group_by
  nest <- tidyr::nest

  d0 <- predictors[numrow,]

  xid <- d0$id
  xraster <- d0$raster

  dat0 <- d0$data %>% data.frame()

  sim0 <- predict(rfmodel, dat0) %>% data.frame() %>% rename(pred=1)  %>%
    bind_cols(dat0) %>%
    mutate(id = xid, raster = xraster)  %>%
    select(raster, id, x, y, pred)

  n <-  sim0 %>% group_by(raster, id) %>% nest() %>% mutate(numrow = numrow)

  return(n)
  rm(d0, dat0, sim0)
}


