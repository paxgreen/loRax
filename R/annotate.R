




annotate <- function(xid){


  filter <- dplyr::filter
  crop <- raster::crop
  stack <- raster::stack
  ggRGB <- RStoolbox::ggRGB
  ntile <- dplyr::ntile
  mutate <- dplyr::mutate
  group_by <- dplyr::group_by
  select <- dplyr::select

  coords <- filter(annotation_grid, id == xid)
  xmin <- coords$xmin; xmax <- coords$xmax
  ymin <- coords$ymin; ymax <- coords$ymax
  ext <- c(xmin, xmax, ymin, ymax)
  ras0 <- raster::crop(myraster, ext)
  ##=========================================
  b2 <- ras0[[2]];b3 <- ras0[[3]];b4 <- ras0[[4]]
  sentinel2 <<- stack(list(b4,b3,b2))
  sentinel2 <<- ggRGB(sentinel2, r = 1, g = 2, b = 3,stretch = "sqrt"
                      , quantiles = c(0.15, 0.85))

  df0 <- mypredictors %>% group_by(id) %>%
    filter(id == xid)
  df1 <- df0$data %>% data.frame() %>%
    mutate(
      b_bsi = ntile(p_bsi,99)
      , b_mndwi = ntile(p_mndwi,99)
      , b_nbai = ntile(p_nbai,99)
      , b_ndbi = ntile(p_ndbi,99)
      , b_ndsi = ntile(p_ndsi,99)
      , b_ndvi = ntile(p_ndvi,99)
      , b_ndwi = ntile(p_ndwi,99)
      , b_npcri = ntile(p_npcri,99)
      , b_si = ntile(p_si,99)
    ) %>%
    select(x, y, b_ndvi, b_npcri, b_si
           ,b_ndsi, b_ndwi, b_mndwi, b_bsi, b_nbai, b_ndbi)

  ###############################

  id <- xid
  apath <- paste0(mydir, 'Annotations/')
  created <- gsub("-", "_", Sys.Date())

  classify(sentinel2, df1, id, apath, myrastername, created)
}
