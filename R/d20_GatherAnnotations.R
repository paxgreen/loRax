
GatherAnnotations <- function(){

separate <- tidyr::separate
mutate <- dplyr::mutate
select <- dplyr::select
distinct <- dplyr::distinct
nest <- tidyr::nest



filelist <- list.files(paste0(mydir, 'Annotations')) %>% data.frame() %>% rename(files = 1)


f0 <- filelist %>%
  mutate(split = files) %>%
  separate(split
  , c('raster', 'place', 'monthday', 'year', 'id', 'id1', 'id2', 'place2'
, 'monthday2', 'year2', 'class', 'annotation', 'created', 'year3', 'month3', 'day3','rds'))

f1 <- f0 %>%
  mutate(
    raster = paste0(place,'_',monthday, '_', year)
    ) %>%
  select(files, raster)

edf <- data.frame()
for (k in 1:nrow(f1)){
d0 <- f1[k,]
d2 <- readRDS(paste0(mydir, 'Annotations/', d0[1,1])) %>%
  mutate(raster = d0[1,2]) %>%
  mutate(x = round(x,5), y = round(y,5))

xid <- as.character(distinct(d2, id))

dat0 <- predictors %>%
  group_by(id) %>%
  filter(id == xid)
dat1 <- dat0$data %>% data.frame() %>%
  mutate(x = round(x,5), y = round(y,5))

dat2 <- d2 %>%
  left_join(dat1, by = c('x', 'y'))

edf <- rbind(edf, dat2)
}

xmin <- min(edf$x)
xmax <- max(edf$x)
ymin <- min(edf$y)
ymax <- max(edf$y)

minmax <- c(xmin, xmax, ymin, ymax)
saveRDS(minmax, paste0(mydir, 'annotation_extent.rds'))

n <- edf %>% group_by(raster, id) %>% nest()

return(n)
}





