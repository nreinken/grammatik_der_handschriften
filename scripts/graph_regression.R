#graph_regression.R
#analysis of graphical letter forms and their correlations with grammatical structures, using regression models
#based on scripts by Niklas Reinken, July 2021
#version 1, January 2023

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(plyr)){install.packages("plyr")}
if(!require(MASS)){install.packages("MASS")}
if(!require(descr)){install.packages("descr")}
library(tidyverse)

source("scripts/dataHandling.R")
source("scripts/regressions.R")

options(scipen = 999)

#Syntagmatic Variation ====
#load data
d_syn <- data.loadData(whichColumns =c("person_ID", "word_index", "junc_border", "letter_freq"), removeWaZ = T, removeWordEnds = T, removeUpperCase = F, removeUnrecognisable = T)

#factorize person ID
d_syn$person_ID <- factor(d_syn$person_ID)

#set up test and training samples
d_syn.split <- split_set(d_syn)
d_syn.train <- d_syn.split$train.data
d_syn.test <- d_syn.split$test.data
rm(d_syn.split)

#set up the model
formula <- as.formula(junc_border ~ person_ID + log(word_index) + log(letter_freq))
full.model <- glm(formula = formula, data = d_syn.train, family = binomial)

#get the best model (stepwise selection)
best.model <- full.model %>% MASS::stepAIC(direction = "both") 

#check for outliers
outliers <- checkOutliers(best.model)

#remove outliers and set up a new model
if(!is.null(outliers))
{
  print("Outliers detected; omitting overly influential cases and setting up new model")
  d_syn.train <- d_syn.train[-outliers, ]
  full.model <- glm(formula = formula, data = d_syn.train, family = binomial)
  best.model <- full.model %>% MASS::stepAIC(direction = "both")
}
rm(outliers)

#check assumptions
checkAssumptions(best.model, d_syn) #!!!CAUTION, this takes some time!!!

#write coefficients to file
write.csv(coef(best.model), "coefs_syntagmatic.csv")

#evaluate model
summary(best.model)
crossvalidate(best.model, d_syn.test)
descr::LogRegR2(best.model)

#check interactions

#set up models with interactions
model.int1 <- glm(junc_border ~ person_ID * log(word_index) + log(letter_freq),
                  data = d_syn.train, family = binomial)
model.int2 <- glm(junc_border ~ person_ID * log(letter_freq) + log(word_index),
                  data = d_syn.train, family = binomial)

#are the models with interactions better than the model without?
anova(best.model, model.int1, model.int2, test = "Chisq")

#model.int1 is slightly better than the default model
summary(model.int1)
LogRegR2(model.int1)

#clean up
rm(best.model, d_syn, d_syn.test, d_syn.train, full.model, model.int1, model.int2, formula)

#Paradigmatic variation ====

#load data
d_par <- data.loadData(whichColumns=c("code", "letter", "person_ID", "word_index", "letter_freq"), removeWaZ = F, removeWordEnds = F, removeUpperCase = T, removeUnrecognisable = F)

#factorize person ID
d_par$person_ID <- factor(d_par$person_ID)

#recode umlauts as base letters
d_par$letter <- plyr::revalue(d_par$letter, c("ä" = "a", "ö" = "o", "ü" = "u"))
d_par <- droplevels(d_par)

#set up a dataframe for result collection
predictRates <- data.frame()

#set up paradigmatic variation models for each letter in list
letters <- sort(unique(d_par$letter))
for(char in letters)
{
  print(char)
  rate <- paraModel(char, data = d_par)
  #add prediction rate to data frame
  predictRates <- rbind(predictRates, rate)
}

#show the results and store them to .csv
colnames(predictRates) <- c("letter", "predictRate")
print(predictRates)
write.csv(predictRates, "predictionRates_paradigmatic.csv")

#clean up
rm(d_par, predictRates, char, letters, rate)
