
CreateCluster <- function(numrow){
  ##=======================================================
  mutate <- dplyr::mutate
  select <- dplyr::select
  distinct <- dplyr::distinct
  arrange <- dplyr::arrange
  left_join <- dplyr::left_join
  future_map_dfr <- furrr::future_map_dfr
  nest <- tidyr::nest
  group_by <- dplyr::group_by
  `%>%` <- magrittr::`%>%`
  row_number <- dplyr::row_number



  d1 <- myelevation[numrow,]

  xraster <-  d1$raster
  xid <- d1$id

  dat0 <- d1$data %>% data.frame()  %>%
    mutate(raster = xraster, id = xid) %>%
    mutate(rownum2 = row_number())

  dat1 <- dat0 %>%
    select(x,y,p_ndvi:p_ndbi)
  ##############
  dy <- dat1 %>%
    distinct(y) %>%
    arrange(y) %>% mutate(yrow = seq(1:nrow(.)))
  ##
  m0 <- dat1 %>%
    left_join(dy, by = 'y')
  ########
  dx <- dat1 %>%
    distinct(x) %>%
    arrange(x) %>% mutate(xcol = seq(1:nrow(.)))
  ##
  m1 <<- m0 %>%
    left_join(dx, by = 'x') %>%
    mutate(rownum3 = row_number())

  res100 <- suppressWarnings(future_map_dfr(1:nrow(m1), ~ GetMedian(.x)))

  dat2 <- dat0 %>%
    left_join(res100, by = c('rownum2' = 'rownum'))

  n <-  dat2 %>% group_by(raster, id,) %>% nest() %>% mutate(numrow = numrow)
  rm(m1, d1, dat0, dat1, dat2, m2)
  return(n)
}
