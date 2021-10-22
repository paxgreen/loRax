GetMedian <- function(rownum3){

  `%>%` <- magrittr::`%>%`
  filter <- dplyr::filter
  select <- dplyr::select
  summarise <- dplyr::summarise
  rename_all <- dplyr::rename_all
  mutate <- dplyr::mutate
  str_replace_all <- stringr::str_replace_all
  funs <- dplyr::funs

  tile_stat <- list(
    med = ~median(.x, na.rm = TRUE)
    ,var = ~var(.x, na.rm = TRUE))
  ############################


  g0 <- m1[rownum3,]
  yrow0 <- g0[1,13]
  xcol0 <- g0[1,14]
  m2 <- m1 %>%
    filter(
      xcol == xcol0 & yrow == yrow0 - 1 |
        xcol == xcol0 - 1 & yrow == yrow0 - 1 |
        xcol == xcol0 + 1 & yrow == yrow0 - 1 |
        xcol == xcol0 & yrow == yrow0|
        xcol == xcol0 - 1 & yrow == yrow0 |
        xcol == xcol0 + 1 & yrow == yrow0 |
        xcol == xcol0 & yrow == yrow0 + 1|
        xcol == xcol0 - 1 & yrow == yrow0 + 1 |
        xcol == xcol0 + 1 & yrow == yrow0 + 1
    ) %>%
    select(-x, -y, -yrow, -xcol, -rownum3) %>%
    summarise(across(where(is.numeric), tile_stat)) %>%
    rename_all(funs(str_replace_all(., "p_", "c_"))) %>%
    mutate(rownum = rownum3)
  rm(g0)
  return(m2)
}
