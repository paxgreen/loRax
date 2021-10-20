


CreateTile <- function(rownum){
  #################
  select <- dplyr::select
  summarise <- dplyr::summarise
  rename_all <- dplyr::rename_all
  left_join <- dplyr::left_join
  nest <- tidyr::nest
  str_replace_all <- stringr::str_replace_all


  tile_stat <- list(
    tile_med = ~median(.x, na.rm = TRUE)
    ,tile_var = ~var(.x, na.rm = TRUE)
  )

  #########
  d1 <- myindex[rownum,]

  xraster <-  d1$raster
  xid <- d1$id
  df0 <- d1$data %>% data.frame()  %>%
    mutate(raster = xraster, id = xid)


  dat0 <- df0 %>% select(p_bsi:p_si) %>%
    summarise(across(where(is.numeric)
                     , tile_stat))  %>%
    rename_all(funs(str_replace_all(., "p_", "t_"))) %>%
    rename_all(funs(str_replace_all(., "tile_", ""))) %>%
    mutate(raster = xraster, id = xid)

  df1 <- df0 %>%
    left_join(dat0, by = c('id','raster'))

  n <-  df1 %>% group_by(raster, id,) %>% nest() %>% mutate(rownum = rownum)

  return(n)
  rm(df0, dat0, df1, d1)
}


