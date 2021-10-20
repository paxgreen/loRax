if(!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, raster, furrr, tictoc, purrr)
select <- dplyr::select
cores <- availableCores()
plan(multicore, workers = (cores - 1)) # this uses the package furrr to parallelize our computations.

##=================================================


mydir <- (path of directory where youve downloaded data from dropbox). # make sure you add a final forward slash at the end, as in "C:/Mindo/"
myrastername <- "mindo_july05_2021"
myraster <- raster::stack(paste0(mydir, myrastername, '.tif')) # store Mindo raster into myraster
myextent <- c(-78.80454, -78.73816, -0.1033067, -0.03692091)

mydata <- loadRData(paste0(mydir, 'MindoData.rda'))

##=================================================
tic()
mygrid <- CreateGrid(myraster, myrastername, myextent)
saveRDS(mygrid, paste0(mydir, 'MindoGrid.rds'))
toc()
tic()
mybandvalues <- future_map_dfr(1:nrow(mygrid) , ~ CreateBandValues(.x))
saveRDS(mybandvalues, paste0(mydir, 'MindoBandValues.rds'))
toc()
minmax <- GetMinMax(mydata)

tic()
myindex <- future_map_dfr(1:nrow(mybandvalues), ~ CreateIndex(.x))
toc()


saveRDS(myindex, paste0(mydir, 'MindoIndex.rds'))

tic()
mytile <- future_map_dfr(1:nrow(mybandvalues), ~ CreateTile(.x))
saveRDS(mytile, paste0(mydir, 'MindoTile.rds'))
toc()

raselevation <- raster(paste0(mydir, 'MindoElevation.tif'))

tic()
myelevation <- future_map_dfr(1:nrow(mybandvalues), ~ CreateElevation(.x))
saveRDS(myelevation, paste0(mydir, 'MindoElevation.rds'))
toc()

mypredictors <- future_map_dfr(1:nrow(myelevation), ~ CreateCluster(.x))
saveRDS(mypredictors, paste0(mydir, 'MindoPredictors.rds'))

## ========================================================================================
mypredictors <- readRDS(paste0(mydir, 'MindoPredictors.rds'))
print(mypredictors)
numrow <- 5
mydataframe <- GetDataFrame(numrow)
print(head(mydataframe))

##========================================================================================
mydata <- loadRData(paste0(mydir, 'MindoData.rda')) # load the blockchained annotated dataset
validateChain(mydata) # this should return "TRUE" if no one

ProcessAnnotationsData(mydata)

print(meta) # provides information about the annotated data and who did the annotations
print(annotationrasters) # Sentinel files whence came the rasters that were annotated
print(head(annotations)) # first six rows of annotated dataset

# the following lines sum the number of pixels annotated per class
tally <- annotations %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  data.frame() %>%
  arrange(count)
print(tally)

# respectively, n_train and n_test are the number of pixels for the training and testing dataset.
# loRax computes 70% and 30% of the total of the class with the least annotations
lowest_count <- tally[1,2]
n_train <- round(0.7 * lowest_count, 0)
n_test <- round(0.3 * lowest_count, 0)
# for classes with sufficient data, we are oversampling.
# if you prefer not to oversample, just set both of these to zero
o_train <- tally[2,2] - (n_train + n_test)
o_train2 <- 5000
tic()
rfmodel <- RunRandomForest(annotations) # about 15 minutes
save(rfmodel, file = paste0(mydir, 'MindoModel.rda'))
toc()
ConfusionMatrix(rfmodel, testdata, n_test)


##=====================================================
predictors <- readRDS(paste0(mydir, 'MindoPredictors.rds'))
tic()
mysimulation <- map_dfr(1:nrow(predictors), ~ CreateSimulation(.x))
saveRDS(mysimulation, paste0(mydir, 'MindoSimulation.rds'))
toc()

##===================================================================
## we need to first upload the sentinel and high-resolution satellite rasters

myrastername <- "mindo_july05_2021"
highresSatelliteName <- 'AirBusRaster'
myraster <- raster::stack(paste0(mydir, myrastername, '.tif'))
hi_res <- raster::stack(paste0(mydir, highresSatelliteName, '.tif'))


## To crop the rasters, the application will need the raster grid you created earlier.
## For this vignette, we'll select tiles for which we have the corresponding high-resolution Airbus images.

annotation_grid <- readRDS(paste0(mydir, 'MindoGrid.rds')) %>%
  mutate(rowx = as.numeric(as.character(rowx)), colx = as.numeric(as.character(colx))) %>%
  filter(rowx >= 8) %>%
  filter(rowx <= 12) %>%
  filter(colx >= 7) %>%
  filter(colx <= 11)
print(annotation_grid)

## You can select a tile by its row number. In this exampel, we'll use row 14.
## In practice, you'll probably want to develop a loop function to annotate groups of tiles at a time.

row <- 14
xid <- annotation_grid[row,1]

## Plot the high-resolution tile image.
plothighres(xid)

## This launches the Shiny application. The code will create a folder in mydir called Annotations to store
## your annotations.

annotate(xid)



