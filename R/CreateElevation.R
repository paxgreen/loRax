

CreateElevation <- function (numrow){

  mutate <- dplyr::mutate
  crop <- raster::crop
  rasterToPoints <- raster::rasterToPoints
  rename <- dplyr::rename
  filter <- dplyr::filter
  left_join <- dplyr::left_join
  nest <- tidyr::nest
  group_by <- dplyr::group_by
  `%>%` <- magrittr::`%>%`


  d1 <- mytile[numrow,]
  xraster <-  d1$raster
  xid <- d1$id
  xgrid <- mygrid %>% filter(id == xid)

  dat0 <- d1$data %>% data.frame() %>%
    mutate(rownum = row_number())

  xmin2 <- xgrid$xmin; xmax2 <- xgrid$xmax
  ymin2 <- xgrid$ymin; ymax2 <- xgrid$ymax
  ex <- c(xmin2,xmax2,ymin2,ymax2)

  rclip <- crop(raselevation,ex)

  elev <- LidarProcessoR::gapfill(rclip
                                  , fill.window.size = 10, fill.window.type = "rectangle",
                                  fill.value = "mean") %>%
    rasterToPoints() %>% data.frame() %>%
    rename(elev = 3) %>%
    mutate(rownum = row_number())

  dat1 <- dat0 %>%
    left_join(elev, by = 'rownum') %>%
    filter(complete.cases(.)) %>%
    mutate(raster = xraster, id = xid, p_elevation = log(elev)) %>%
    select(-rownum, -x.y, -y.y) %>%
    rename(x = x.x, y = y.x)

  n <-  dat1 %>% group_by(raster, id,) %>% nest() %>% mutate(numrow = numrow)
  rm(dat1, elev, dat0,d1)
  return(n)
}

