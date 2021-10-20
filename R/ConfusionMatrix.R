
ConfusionMatrix <- function(rfmodel, testdata, n_test){
  predict <- stats::predict
  rename <- dplyr::rename
  select <- dplyr::select
  filter <- dplyr::filter
  group_by <- dplyr::group_by
  summarise <- dplyr::summarise
  arrange <- dplyr::arrange
  mutate <- dplyr::mutate
  ggplot <- ggplot2::ggplot
  geom_point <- ggplot2::geom_point
  geom_text <- ggplot2::geom_text
  theme_bw <- ggplot2::theme_bw
  aes <- ggplot2::aes
  xlab <- ggplot2::xlab
  ylab <- ggplot2::ylab
  coord_equal <- ggplot2::coord_equal

  pred <- predict(rfmodel, testdata) %>% data.frame() %>% rename(pred=1)
  actual <- testdata %>% select(x,y,class) %>% rename(actual = class)

  check <- cbind(actual,pred) %>%
    mutate(pred = as.character(pred),actual = as.character(actual)) %>%
    mutate(match = ifelse(actual == pred,1,0))

  #####################################
  ############################
  k <- n_test

  right <- check %>%
    filter(match == 1)%>%
    group_by(actual,pred) %>%
    summarise(count = n()) %>%
    data.frame() %>%
    arrange(desc(count)) %>%
    mutate(prop = (count/k)*100) %>%
    mutate(prop = sprintf('%.2f', round(prop,2))) %>%
    mutate(xlabel = paste0(prop, '\n of ', k))

  wrong <- check %>%
    filter(match == 0) %>%
    group_by(actual,pred) %>%
    summarise(count = n()) %>%
    data.frame() %>%
    arrange(desc(count)) %>%
    mutate(prop2 = (count/k)*100)  %>%
    mutate(prop2 = round(prop2,2)) %>%
    filter(prop2 >= 1 )  %>%
    mutate(prop2 = sprintf('%.2f', round(prop2,2))) %>%
    mutate(xlabel = paste0(prop2, '\n of ', k))
  ########################################

  ################################################
  ssize1 <- 21
  fsize1 <- 3.0
  ssize2 <- 16
  fsize2 <- 2.5

  p <- ggplot(data = right) + theme_bw()
  p1 <- p + geom_point(aes(pred,actual),shape=22,size=ssize1,fill='#ffffcc')
  p2 <- p1 + geom_point(
    data = filter(right, actual == 'tree')
    ,aes(pred, actual), shape=22,size=ssize1,fill='#ccebc5')

  p3 <- p2 + geom_text(data = right, aes(pred,actual,label= xlabel)
                       , size = fsize1)#, fontface = 'bold')

  p4 <- p3 + geom_point(data= wrong, aes(pred,actual),shape=22,size= ssize2,fill='#fcbba1')

  p5 <- p4 + geom_text(data= wrong,aes(pred,actual,label= prop2)
                       , size = fsize2)

  p6 <- p5 +
    xlab("Predicted Class") + ylab("Actual Class") +
    coord_equal()
  windows()
  plot(p6)

  Mean_Decrease_Accuracy <- rfmodel$importance %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    rename(predictor = 1, mda = 8) %>%
    select(predictor, mda) %>%
    arrange(desc(mda))
  print(Mean_Decrease_Accuracy)

}


