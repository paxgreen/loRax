
InputBlockChain <- function(){
  select <- dplyr::select
  mutate <- dplyr::mutate
  rename <- dplyr::rename
  group_by <- dplyr::group_by
  distinct <- dplyr::distinct
  `%>%` <- magrittr::`%>%`

xmin <- annotation_extent[1]
xmax <- annotation_extent[2]
ymin <- annotation_extent[3]
ymax <- annotation_extent[4]
country = 'country'
region = 'region'
firstname = 'firstname'
lastname = 'lastname'
email = 'email'
created = Sys.time()#########
metadata <<- suppressWarnings(data.frame(country, region,xmin, xmax, ymin, ymax, firstname, lastname, email, created) %>%
select(-firstname, -lastname) %>%
tidyr::gather() %>%
mutate(value = ifelse(key == 'created', as.character(Sys.time()), value)) %>%
rename(parameter = 1) %>%
mutate(value = ifelse(parameter == 'xmax' | parameter == 'ymax' | parameter == 'xmin' | parameter == 'ymin'
, as.character(round(as.numeric(as.character(value)),4))
,value
 )))

rasters <<- annotations  %>%
 group_by(raster) %>%
distinct(raster) %>%
mutate(filename = 'filename')
}

