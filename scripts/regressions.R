#regressions.R
#functions for regression analysis
#based on scripts by Niklas Reinken, July 2021
#version 1, January 2023

if(!require(caret)){install.packages("caret")}
if(!require(car)){install.packages("car")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(nnet)){install.packages("nnet")}

#function to split a dataset into 80% training data und 20% test data (based on the variable junc_border)
split_set <- function(dataset)
{
  set.seed(8)
  training.samples <- dataset$junc_border %>% 
    caret::createDataPartition(p = 0.8, list = FALSE)
  train.data  <- droplevels(dataset[training.samples, ])
  test.data <- droplevels(dataset[-training.samples, ])
  return(list("train.data" = train.data, "test.data" = test.data))
}

#function to find outliers and return their row numbers
checkOutliers <- function(model)
{
  cooksd <- cooks.distance(model)
  return(as.numeric(names(cooksd)[(cooksd > 100*mean(cooksd, na.rm=T))]))  # influential row numbers
}

#function to crossvalidate the model with test data
crossvalidate <- function(model, test.data)
{
  # Make predictions
  probabilities <- model %>% predict(test.data, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, T, F)
  
  # Model accuracy
  return(mean(predicted.classes == test.data$junc_border))
  
}

#function to check assumptions for logistic regression models
checkAssumptions <- function(model, data)
{
  #check linearity ====
  probabilities <- model %>% predict(data, type = "response")
  mydata <- data %>%
    dplyr::select_if(is.numeric)
  mydata <- log(mydata)
  predictors <- colnames(mydata)
  
  #plot
  mydata <- mydata %>%
    mutate(logit = log(probabilities/(1-probabilities))) %>%
    gather(key = "predictors", value = "predictor.value", -logit)
  
  plot <- ggplot(mydata, aes(logit, predictor.value)) +
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess", se = F) +
    facet_wrap(~predictors, scales = "free_y") +
    theme_bw()
  print("Check for linearity with graphical analysis, see plot")
  print(plot) #deactivate for testing, takes a long time
  
  
  #check for overly influential data points ====
  model.data <- broom::augment(model) %>%
    dplyr::mutate(index = 1:n())
  model.data %>% top_n(3, .cooksd)
  infl <- model.data %>%
    filter(abs(.std.resid) > 3)
  if(nrow(infl) == 0)
  {
    print("No overly influential data points.")
  }   else
  {
    print("Overly influential data points:")
    print(infl)
  }
  
  #check for multicollinearity ====
  print("Check for multicollinearity:")
  print(car::vif(model))
}

#function to create a multinom model for each letter
paraModel <- function(character, data)
{
  data <- filter(data, data$letter == character)
  data$letter <- NULL
  data <- droplevels(data)
  
  #create train and test data
  set.seed(8)
  training.samples <- data$code %>%
    createDataPartition(p = 0.8, list = F)
  train.data <- data[training.samples,]
  test.data <- data[-training.samples,]
  
  #set up model
  model <- nnet::multinom(code ~ person_ID + word_index + letter_freq,
                          data = train.data,
                          MaxNWts = 9999, model = T)
  #evaluate model
  predicted.classes <- model %>% predict(test.data)
  
  result <- c(character, mean(predicted.classes == test.data$code))
  return(result)
}
 
