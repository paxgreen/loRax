
#' RunRandomForest
#' Part of the loRax package to track tree cover loss, RunRandomForest
#' accepts training data built from loRax to train a Random Forest model.
#' The resulting model can be used to simulate Land Use/Land Cover classes.
#'
#'

RunRandomForest <- function(annotations){
  detectCores <- parallel::detectCores
  cl <- parallel::makeCluster
  detectCores <- parallel::detectCores
  doParallel::registerDoParallel(cl)
  foreach <- foreach::foreach
  randomForest <- randomForest::randomForest
  select <- dplyr::select


  `%>%` <- magrittr::`%>%`

  `%dopar%` <- foreach::  `%dopar%`

  #########
  ###############################################
  trainingdata <- annotations %>%
    group_by(class) %>%
    sample_n(n_train, replace = F) %>%
    data.frame()
  #########
  trainlist <- trainingdata %>%
    distinct(rownum) %>% mutate(train=1)
  ##########
  testdata <<- annotations %>%
    left_join(trainlist)%>%
    filter(is.na(train)) %>%
    select(-train) %>%
    group_by(class) %>%
    sample_n(size= n_test,replace=F) %>%
    data.frame()

  #####################################
  ## OVERSAMPLE
  xlist <- testdata %>%
    distinct(rownum) %>%
    mutate(train = 0) %>%
    bind_rows(trainlist)
  ######

  over <- annotations %>%
    left_join(xlist,by= 'rownum') %>%
    filter(is.na(train)) %>%
    select(-train) %>%
    filter(class == 'built' ) %>%
    group_by(class) %>%
    sample_n(size= o_train,replace=F) %>%
    data.frame()

  ######

  over2 <- annotations %>%
    left_join(xlist,by= 'rownum') %>%
    filter(is.na(train)) %>%
    select(-train) %>%
    filter(
      class == 'tree' | class == 'grass' |
        class == 'soil' )   %>%
    group_by(class) %>%
    sample_n(size= o_train2,replace=F) %>%
    data.frame()

  ##############
  trainingdata2 <- rbind(trainingdata,over, over2) %>%
    mutate(class = as.factor(class))

  rm(annotations,over, over2, trainingdata)
  gc()
  cl <- parallel::makeCluster(detectCores() - 2)
  doParallel::registerDoParallel(cl)
  foreach <- foreach::foreach
  randomForest <- randomForest::randomForest
  cores <- detectCores()
  #########
  ###############################################
  mod <- foreach(ntree=rep(600, cores)
                 , .combine= randomForest::combine
                 , .multicombine=TRUE
                 , .packages='randomForest') %dopar% {
                   randomForest(
                     class ~
                       p_si + p_ndvi + p_b4 + p_b3 + p_b2 + t_si_var + p_ndwi + t_npcri_med + p_b12 + c_ndvi_med + p_b11 + c_ndwi_med + t_mndwi_med + t_ndsi_med + p_mndwi + p_ndsi + p_elevation + c_si_med + t_si_med + t_ndwi_var + t_mndwi_var + t_ndsi_var + t_bsi_med + t_ndwi_med + c_ndwi_var + p_nbai + p_bsi + t_nbai_med + t_ndbi_med + t_ndvi_var + t_nbai_var + t_ndvi_med + t_npcri_var + p_b1 + c_ndvi_var + c_nbai_med + c_ndsi_med + p_b5
                     #################################
                     ,data = trainingdata2
                     ,importance=TRUE
                     , ntree=ntree # the bigger the more accurATE
                     ,.inorder = FALSE
                     , mtry = 7
                     , nodesi = 1   # the bigger the less accurate
                   )}
  #################
  rm(trainingdata2)
  #######################################
  stripRF <- function(cm) {
    cm$finalModel$predicted <- NULL
    cm$finalModel$oob.times <- NULL
    cm$finalModel$y <- NULL
    cm$finalModel$votes <- NULL
    cm$control$indexOut <- NULL
    cm$control$index    <- NULL
    cm$trainingData <- NULL

    attr(cm$terms,".Environment") <- c()
    attr(cm$formula,".Environment") <- c()

    cm
  }
  ############################################
  rfmodel <- stripRF(mod)
  rm(mod)
  return(rfmodel)
}
