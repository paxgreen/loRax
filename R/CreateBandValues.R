
CreateBandValues <- function(rownum){

  ###################################

  rasterToPoints <- raster::rasterToPoints
  crop <- raster::crop
  mutate <- dplyr::mutate
  select <- dplyr::select
  rename <- dplyr::rename
  `%>%` <- magrittr::`%>%`
  filter <- dplyr::filter
  left_join <- dplyr::left_join
  group_by <- dplyr::group_by
  nest <- tidyr::nest

  #res0 <- foreach (k = 1:nrow(mygrid), .combine = rbind) %dopar% {

  d1 <- mygrid[rownum,]

  xid <- d1$id
  coords <- mygrid %>% filter(id == xid)
  xmin <- coords$xmin; xmax <- coords$xmax;
  ymin <- coords$ymin; ymax <- coords$ymax
  ext <- c(xmin,xmax,ymin,ymax)
  rasclip <- crop(myraster, ext)

  b1 <- rasclip[[1]] %>% rasterToPoints() %>% data.frame() %>%
    rename(band1 = 3)
  b2 <- rasclip[[2]] %>% rasterToPoints() %>% data.frame()
  b3 <- rasclip[[3]] %>% rasterToPoints() %>% data.frame()
  b4 <- rasclip[[4]]  %>% rasterToPoints() %>% data.frame()
  b5 <- rasclip[[5]] %>% rasterToPoints() %>% data.frame()
  b6 <- rasclip[[6]]  %>% rasterToPoints() %>% data.frame()
  b7 <- rasclip[[7]]   %>% rasterToPoints() %>% data.frame()
  b8 <- rasclip[[8]]  %>% rasterToPoints() %>% data.frame()
  b8a <- rasclip[[9]]  %>% rasterToPoints() %>% data.frame()
  b9 <- rasclip[[10]] %>% rasterToPoints() %>% data.frame()
  b11 <- rasclip[[11]] %>% rasterToPoints() %>% data.frame()
  b12 <- rasclip[[12]] %>% rasterToPoints() %>% data.frame()
  ###################################################
  d0 <- b1 %>%
    mutate(raster = myrastername, id = xid) %>%
    select(raster, id, x, y, band1) %>%
    left_join(b2,by=c('x','y'))%>%
    left_join(b3,by=c('x','y'))%>%
    left_join(b4,by=c('x','y'))%>%
    left_join(b5,by=c('x','y'))%>%
    left_join(b6,by=c('x','y'))%>%
    left_join(b7,by=c('x','y'))%>%
    left_join(b8,by=c('x','y'))%>%
    left_join(b8a,by=c('x','y'))%>%
    left_join(b9,by=c('x','y'))%>%
    left_join(b11,by=c('x','y'))%>%
    left_join(b12,by=c('x','y'))%>%
    rename(raster = 1, id = 2, x=3,y = 4
           , b1 = 5, b2 =6,b3=7, b4 = 8, b5 =9,b6=10
           , b7 = 11, b8 =12,b8a=13,b9 = 14, b11 =15
           ,b12=16)
  ################
  n <-  d0 %>% group_by(raster, id) %>% nest() %>% mutate(rownum = rownum)
  rm(d0, b1, b2, b3, b4, b5, b6, b7, b8, b8a, b9, b11, b12, rasclip, coords)
  return(n)
}


