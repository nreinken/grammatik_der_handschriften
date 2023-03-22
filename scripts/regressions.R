#regressions.R
#functions for regression analysis
#based on scripts by Niklas Reinken, July 2021
#version 2, March 2023

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
library(broom)
if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet")
}
library(nnet)

#' Function to split a dataset into training and testing data based on a specified split ratio and a group variable
#' Arguments:
#' @param dataset: the dataset to be split
#' @param split_ratio: the ratio of data to be used for training (default 0.8)
#' Returns:
#'  @return A list containing the training and testing data
split_set <- function(dataset, split_ratio = 0.8)
{
  #set seed for reproducibility
  set.seed(8)
  
  #create a vector of random indices for the training data
  train.idx <- caret::createDataPartition(p = split_ratio, list = FALSE, 
                                          y = dataset$junc_border)
 
  #split the dataset into training and testing data
  train.data  <- droplevels(dataset[train.idx, ])
  test.data <- droplevels(dataset[-train.idx, ])
  
  #return the training and testing data as a named list
  return(list("train.data" = train.data, "test.data" = test.data))
}

#' Function to identify influential outliers in a linear regression model
#' Arguments:
#' @param model: the linear regression model to be checked
#' @param threshold: the threshold for Cook's distance to identify influential outliers (default 100 times the mean)
#' Returns:
#' @return A vector of row numbers corresponding to the influential outliers
checkOutliers <- function(model, threshold = 100 * mean(cooks.distance(model), na.rm = TRUE))
{
  # Check whether the input is a valid lm object
  stopifnot(inherits(model, "lm"))
  
  # Compute Cook's distance and identify influential outliers
  cooks_dist <- cooks.distance(model)
  outliers <- which(cooks_dist > threshold)
  
  # Return the row numbers of the influential outliers
  return(outliers)
}

#' Function to cross-validate a model with test data
#' Arguments:
#' @param model: the trained model to be cross-validated
#' @param test.data: the test dataset to use for cross-validation
#' Returns:
#' @return A confusion matrix and a vector of performance metrics for the cross-validated model
crossvalidate <- function(model, test.data)
{
  # Make predictions on the test data
  probabilities <- predict(model, newdata = test.data, type = "response") %>% as.numeric()
  predicted.classes <- factor(ifelse(probabilities > 0.5, TRUE, FALSE))
  
  # Compute performance metrics
  conf_matrix <- confusionMatrix(predicted.classes, test.data$junc_border)
  perf_metrics <- c(conf_matrix$overall["Accuracy"], conf_matrix$byClass["Sensitivity"], 
                    conf_matrix$byClass["Specificity"], conf_matrix$byClass["AUC"])
  
  # Return the confusion matrix and performance metrics
  return(list("Confusion Matrix" = conf_matrix$table, "Performance Metrics" = perf_metrics))
  
}

#' Function to check assumptions for logistic regression models
#' Arguments:
#' @param model: the model to be checked
#' @param data: the data the model is applied to
#' @param generate_plot: a boolean to indicate whether to generate the plots or not; default: TRUE. Generating plots may take a lot of time.
checkAssumptions <- function(model, data, generate_plot = TRUE)
{
  # Check input
  stopifnot(inherits(model, "glm"))
  stopifnot(is.data.frame(data))
  
  # Check predictor variables
  predictors <- names(data)[sapply(data, is.numeric)]
  stopifnot(length(predictors) > 0)
  
  # Check if generate_plot is boolean
  stopifnot(is.logical(generate_plot))
  
  
  # Check linearity assumption
  prob <- predict(model, data, type = "response")
  logit <- log(prob / (1 - prob))
  mydata <- data %>%
    select(predictors) %>%
    mutate(logit = logit) %>%
    pivot_longer(cols = predictors, names_to = "predictors", values_to = "predictor.value")
  
  if (generate_plot) {
    plot <- mydata %>%
      ggplot(aes(x = logit, y = predictor.value)) +
      geom_point(size = 0.5, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE) +
      facet_wrap(~predictors, scales = "free_y") +
      theme_bw()
    print("Check for linearity with graphical analysis, see plot")
    print(plot)
  }
  
  # Check for overly influential data points
  model.data <- broom::augment(model) %>%
    dplyr::mutate(index = 1:n())
  model.data %>% top_n(3, .cooksd)
  infl <- model.data %>%
    filter(abs(.std.resid) > 3)
  
  if (nrow(infl) == 0) {
    print("No overly influential data points.")
  } else {
    print("Overly influential data points:")
    print(infl)
  }
  
  # Check for multicollinearity
  vif_vals <- car::vif(model)
  
  if (all(vif_vals < 10)) {
    print("No multicollinearity detected.")
  } else {
    print("Multicollinearity detected:")
    print(vif_vals)
  }
}

#' Function to create a multinom model for a given letter
#' Arguments:
#' @param character: the character for which the model is to be computed
#' @param data: the data for the model
#' Returns:
#' @return A list of the character and the model accuracy
paraModel <- function(character, data)
{
  cat(paste0(character, ": "))
  # Input validation
  stopifnot(is.character(character), nchar(character) == 1)
  stopifnot(is.data.frame(data), 
            all(c("code", "letter", "person_ID", "word_index", "letter_freq") %in% colnames(data)))
  
  
  # Subset data for given character
  data <- data %>%
    filter(letter == character) %>%
    select(-letter) %>%
    droplevels()
  
  #set seed for reproducibility
  set.seed(8)
  
  #create a vector of random indices for the training data
  training.samples <- data$code %>%
    createDataPartition(p = 0.8, list = F)
  train.data <- data[training.samples,]
  test.data <- data[-training.samples,]
  
  #set up formula
  formula <- as.formula("code ~ person_ID + word_index + letter_freq")
  
  #set up model
  model <- nnet::multinom(formula = formula,
                          data = train.data,
                          MaxNWts = 9999, model = T, trace=F)
  #evaluate model
  predicted.classes <- model %>% predict(test.data)
  mean <- mean(predicted.classes == test.data$code)
  
  cat(paste0(mean, "\n"))
  
  #return result
  result <- c(character, mean)
  return(result)
}

#' Function to extract coefficients from a model
#' Arguments:
#' @param model: the character for which the model is to be computed
#' @param varName: the data for the model
#' @param fileName: the file name to save the results to. Existing files are overwritten.
#' Returns:
#' @returns A dataframe including the significant variable names and the coefficients
getCoefs <- function(model, varName, fileName)
{
  # Input validation
  stopifnot(inherits(model, "glm"))
  stopifnot(is.character(varName) & nchar(varName) > 0)
  stopifnot(is.character(fileName) & nchar(fileName) > 0)
  
  # Round coefficients to 3 decimal places and format as data frame
  coefs <- round(model$coefficients,3)
  coefs <- data.frame(coefs)
  coefs <- cbind(rownames(coefs), data.frame(coefs, row.names=NULL))
  
  # Remove variable name prefix
  coefs$names <- str_replace_all(coefs$`rownames(coefs)`, varName, "")
  coefs$`rownames(coefs)` <- NULL
  
  # Filter for significant variables based on p-values
  pvalues <- summary(model)$coeff[-1, 4]
  toselect <- pvalues < 0.05
  coefs <- coefs[toselect, ]
  
  # Omit rows with NA values
  coefs <- na.omit(coefs)
  
  # Write coefficients to CSV file
  if (!dir.exists("results")) {
    dir.create("results")
  }
  write.csv2(coefs, file.path("results", paste0("coefficients_", fileName, ".csv")), row.names = FALSE)
  
  return(coefs)
}
 
