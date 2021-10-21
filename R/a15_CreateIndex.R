


CreateIndex <- function(rownum){

  mutate <- dplyr::mutate
  group_by <- dplyr::group_by
  nest <- tidyr::nest
  `%>%` <- magrittr::`%>%`

  d1 <- mybandvalues[rownum,]

  xid <- d1$id
  xraster <- d1$raster
  df0 <- d1$data %>% data.frame()

  bands <- df0 %>%
    mutate(
      p_b1 = ((b1 - minmax$min_b1)/(minmax$max_b1 - minmax$min_b1))
      ,p_b2 = ((b2 - minmax$min_b2)/(minmax$max_b2 - minmax$min_b2))
      ,p_b3 = ((b3 - minmax$min_b3)/(minmax$max_b3 - minmax$min_b3))
      ,p_b4 = ((b4 - minmax$min_b4)/(minmax$max_b4 - minmax$min_b4))
      ,p_b5 = ((b5 - minmax$min_b5)/(minmax$max_b5 - minmax$min_b5))
      ,p_b6 = ((b6 - minmax$min_b6)/(minmax$max_b6 - minmax$min_b6))
      ,p_b7 = ((b7 - minmax$min_b7)/(minmax$max_b7 - minmax$min_b7))
      ,p_b8 = ((b8 - minmax$min_b8)/(minmax$max_b8 - minmax$min_b8))
      ,p_b8a = ((b8a - minmax$min_b8a)/(minmax$max_b8a - minmax$min_b8a))
      ,p_b9 = ((b9 - minmax$min_b9)/(minmax$max_b9 - minmax$min_b9))
      ,p_b11 = ((b11 - minmax$min_b11)/(minmax$max_b11 - minmax$min_b11))
      ,p_b12 = ((b12 - minmax$min_b12)/(minmax$max_b12 - minmax$min_b12))
    )


  index <- bands %>%
    mutate(
      p_ndvi = (b8 - b4)/(b8 + b4)
      ,p_npcri = (b4 - b2)/(b4 + b2)
      ,p_si = sqrt((256 - b2) * (256 - b3))
      ,p_ndsi = (b4 - b6)/(b4 + b6)
      ,p_ndwi = (b3 - b8)/(b3 + b8)
      ,p_mndwi = (b3 - b11)/(b3 + b11)
      ,p_bsi = ((b11 + b4)-(b8 + b2))/((b11 + b4)+(b8 + b2))
      ,p_nbai = ((b12 - b8)/b2)/((b12 + b8)/b2)
      ,p_ndbi = (b11 - b8)/(b11 + b8)
    ) %>%
    mutate(raster = myrastername, id = xid)

  n <- index %>% group_by(raster, id,) %>% nest() %>% mutate(rownum = rownum)
  #############
  return(n)
}


