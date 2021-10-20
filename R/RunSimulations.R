
rm(list=ls())
gc()
.libPaths('c:/_data/Rpackages')
pacman::p_load(dplyr,raster,tidyr, stringr, reshape2, randomForest)
select <- dplyr::select
pacman::p_load(parallel,foreach,doParallel,dplyr,raster, purrr, magrittr)
detectCores()
summarise <- dplyr::summarise
rename <- dplyr::rename
##########################################################

pred0 <- readRDS('F:/cloudforest/expt0/data/predictors.rds')

pred1 <- pred0 %>%
  group_by(raster) %>%
  #filter(raster == 'mindo_july05_2021')
  #filter(raster == 'mindo_aug24_2020')
  filter(raster == 'mindo_aug30_2019')
############################################
edf <- data.frame()

for (k in 1:nrow(pred1)){

  df1 <- pred1[k,]
  id0 <- df1$id
  raster0 <- df1$raster

  dat0 <- df1$data %>% data.frame()

  sim0 <- predict(mod0, dat0) %>% data.frame() %>% rename(pred=1)  %>%
    bind_cols(dat0) %>%
    mutate(id = id0, raster = raster0)  %>%
    select(raster, id, x, y, pred)

  edf <- rbind(edf, sim0)
  print(k)
  gc()
}

saveRDS(edf, paste0('F:/cloudforest/expt0/data/mindosim_', raster0, '.rds'))

