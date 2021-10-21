plothighres <- function(xid){

  crop <- raster::crop
  stack <- raster::stack
  ggRGB <- RStoolbox::ggRGB
  coord_equal <- ggplot2::coord_equal
  theme_bw <-ggplot2::theme_bw

  coords <- annotation_grid %>% filter(id == xid)
  ext <- c(coords$xmin, coords$xmax, coords$ymin, coords$ymax)

  h0 <- crop(hi_res, ext)

  b1 <- h0[[1]]; b2 <- h0[[2]]; b3 <- h0[[3]]
  h0 <- stack(list(b3,b2,b1))
  h0 <- ggRGB(h0, r = 3, g = 2, b = 1,stretch = "sqrt"
              , quantiles = c(0.02, 0.98)) +
    coord_equal() +
    theme_bw()
  windows()
  plot(h0)
}
