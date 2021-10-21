

CreateGrid <- function(myraster, myrastername, myextent){
  ##=========================================================
  ##================= CROP RASTER AND GET COORDINATES


  ras1 <- raster::crop(myraster, myextent)
  D2 <- 0.0034233 # CONVERSION FACTOR
  ##=================== CREATE GRID
  gridx <- raster::raster(raster::extent(ras1))
  raster::res(gridx) <- D2
  ## CREATE GRID
  sp::proj4string(gridx) <- sp::proj4string(ras1)
  RegionGrid <- raster::rasterToPolygons(gridx)

  RegionGrid@data$id <- rownames(RegionGrid@data)
  fortified <- ggplot2::fortify(RegionGrid, region = "id")
  #### CREATE DATA FRAME
  RegionGrid_df <- dplyr::left_join(fortified, RegionGrid@data, by = "id")
  RegionGrid_df <- dplyr::arrange(RegionGrid_df,id)

  df0 <- dplyr::group_by(RegionGrid_df, group)
  df0 <-  dplyr::summarize(df0,
                           xmin = min(long)
                           ,xmax = max(long)
                           ,ymin = min(lat)
                           ,ymax = max(lat)
  )

  df0 <- as.data.frame(df0)
  df0 <- dplyr::arrange(df0, desc(ymax), xmin)


  xrows <- dplyr::group_by(df0, ymax)
  xrows <- dplyr::summarise(xrows, rowcount = dplyr::n())
  xrows <- dplyr::arrange(xrows, dplyr::desc(ymax))
  xrows <- dplyr::mutate(xrows, rowx = rownames(xrows))

  xcols <- dplyr::group_by(df0, xmin)
  xcols <- dplyr::summarise(xcols, colcount = dplyr::n())
  xcols <- dplyr::arrange(xcols, dplyr::desc(xmin))
  xcols <- dplyr::mutate(xcols, colx = rownames(xcols))



  df1 <- dplyr::left_join(df0, xrows,by='ymax')
  df1 <- dplyr::left_join(df1, xcols,by='xmin')
  df1 <- dplyr::arrange(df1, as.numeric(rowx), as.numeric(colx))
  df1 <- dplyr::mutate(df1, id = paste0(rowx,'_',colx,'_',myrastername))
  df1 <- dplyr::select(df1, id, xmin, xmax, ymin, ymax, rowx, colx)


  df2 <- dplyr::mutate(df1, across(xmin:ymax, ~round(.x, 6)))
  df2 <- dplyr::mutate(df2, rownum = dplyr::row_number())

  return(df2)
}
