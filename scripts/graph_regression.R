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
source("scripts/graphics.R")

options(scipen = 999)

#Syntagmatic variation ====
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
if(!is_empty(outliers))
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
write.csv(coef(best.model), "results/coefs_syntagmatic.csv")

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
write.csv(predictRates, "results/predictionRates_paradigmatic.csv")

#clean up
rm(d_par, predictRates, char, letters, rate)

#Junctions and letters ====
#load data
d_letters <- data.loadData(whichColumns = c("junc_border", "next_letter", "prev_letter"), 
                           removeWaZ = T, removeWordEnds = T, removeUpperCase = T, removeUnrecognisable = F)

#remove upper case letters in prev_letter
d_letters <- droplevels(filter(d_letters, !prev_letter %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", 
                                     "J", "K", "L", "M", "N", "O", "P", "Q", "R", 
                                     "S", "T", "U", "V", "W", "X", "Y", "Z", "Ä", "Ö", "Ü")))


#set up test and training samples
d_letters.split <- split_set(d_letters)
d_letters.train <- d_letters.split$train.data
d_letters.test <- d_letters.split$test.data
rm(d_letters.split)

#set up model
formula <- formula(junc_border ~ next_letter + prev_letter)
full.model <- glm(formula, data = d_letters.train, family = binomial())
best.model <- full.model %>% stepAIC(direction = "both")

#check for outliers
outliers <- checkOutliers(best.model)

#remove outliers and set up a new model
if(!is_empty(outliers))
{
  print("Outliers detected; omitting overly influential cases and setting up new model")
  d_letters.train <- d_letters.train[-outliers, ]
  full.model <- glm(formula = formula, data = d_letters.train, family = binomial)
  best.model <- full.model %>% MASS::stepAIC(direction = "both")
}
rm(outliers)

#there are no numeric variables in the model, so no assumptions can be checked

#calculate significant coefficients
coefs <- getCoefs(best.model, varName = "next_letter", fileName = "letters")

#prepare coefficients for plotting
coefs_ext <- arrange(coefs, desc(coefs))
coefs_ext <- mutate(coefs_ext, color = ifelse(str_detect(rownames.coefs.,"prev_letter"), "previousLetter", "followingLetter"))
coefs_ext$rownames.coefs. <- str_remove_all(coefs_ext$rownames.coefs.,"prev_letter")
coefs_ext$rownames.coefs. <- str_remove_all(coefs_ext$rownames.coefs.,"next_letter")
coefs_ext <- mutate(coefs_ext, name = rownames.coefs.)
coefs_ext <- mutate(coefs_ext, pos = ifelse(coefs > 0, coefs + 0.7, coefs - 0.7))
coefs_ext <- arrange(coefs_ext, desc(coefs))

#split up into datasets for previous and following letter
split.set <- split(coefs_ext, coefs_ext$color)

#plot both coefficients sets
plot_coefs(split.set$followingLetter, name = "followingLetter")
plot_coefs(split.set$previousLetter, name = "previousLetter")

rm(coefs, coefs_ext)

summary(best.model)
crossvalidate(best.model, d_letters.test)
descr::LogRegR2(best.model)

#interactions
model.int <- glm(junc_border ~ next_letter * prev_letter,
                 data = d_letters.train, family = binomial) #!!!CAUTION!!! takes a lot of time

#is the model with interactions better than the model without?
anova(best.model, model.int, test = "Chisq") #it's not better

#clean up
rm(best.model, coefs, coefs_ext, d_letters, d_letters.test, d_letters.train, full.model, model.int, split.set, formula, toselect)

#Junctions and letterforms ====
#load data
d_forms <- data.loadData(whichColumns = c("code","junc_border"), 
                         removeWaZ = T, removeWordEnds = T, removeUpperCase = T, removeUnrecognisable = T)

#set up test and training samples
d_forms.split <- split_set(d_forms)
d_forms.train <- d_forms.split$train.data
d_forms.test <- d_forms.split$test.data
rm(d_forms.split)

#set up model
formula <- formula(junc_border ~ code)
full.model <- glm(formula, data = d_forms.train, family = binomial())
best.model <- full.model %>% stepAIC(direction = "both")

#check for outliers
outliers <- checkOutliers(best.model)

#remove outliers and set up a new model
if(!is_empty(outliers))
{
  print("Outliers detected; omitting overly influential cases and setting up new model")
  d_forms.train <- d_forms.train[-outliers, ]
  full.model <- glm(formula = formula, data = d_forms.train, family = binomial)
  best.model <- full.model %>% MASS::stepAIC(direction = "both")
}
rm(outliers)

#there are no numeric variables in the model, so no assumptions can be checked

#calculate coefficients
coefs <- getCoefs(best.model, varName = "code", fileName = "letterforms")

#prepare coefs for plotting
coefs_ext <- arrange(coefs, desc(coefs))
coefs_ext$rownames.coefs. <- str_remove_all(coefs_ext$rownames.coefs.,"code")
coefs_ext <- mutate(coefs_ext, name = rownames.coefs.)
coefs_ext <- mutate(coefs_ext, pos = ifelse(coefs > 0, coefs + 1.5, coefs - 1.5))
coefs_ext <- arrange(coefs_ext, coefs)
coefs_ext$rownames.coefs. <- NULL
max_coefs <- head(arrange(coefs_ext, desc(coefs)),15)
min_coefs <- tail(arrange(coefs_ext, desc(coefs)),15)
coefs_ext <- rbind(max_coefs, min_coefs)
names(coefs_ext)[2] <- "rownames.coefs."

#clean up
rm(max_coefs, min_coefs)

#plot
plot_coefs(coefs_ext, name = "letterforms")

#evaluate model
summary(best.model)
crossvalidate(best.model, d_forms.test)
descr::LogRegR2(best.model)

#clean up
rm(best.model, coefs, coefs_ext, d_forms, d_forms.test, d_forms.train, full.model, formula)

#Junctions and bigrams ====
#load data
d_bigr <- data.loadData(whichColumns = c("junc_border", "bigramm_next"), removeWaZ = T, removeWordEnds = T, removeUpperCase = T, removeUnrecognisable = F)

#filter bigrams with a low frequency (less than 100)
freqs <- data.frame(table(d_bigr$bigramm_next))
low_freqs <- filter(freqs, Freq < 100)
d_bigr <- droplevels(filter(d_bigr, !bigramm_next %in% low_freqs$Var1))
rm(freqs, low_freqs)

#set up test and training samples
d_bigr.split <- split_set(d_bigr)
d_bigr.train <- d_bigr.split$train.data
d_bigr.test <- d_bigr.split$test.data
rm(d_bigr.split)

#set up model
formula <- formula(junc_border ~ bigramm_next)
full.model <- glm(formula, data = d_bigr.train, family = binomial())
best.model <- full.model %>% stepAIC(direction = "both")

#check for outliers
outliers <- checkOutliers(best.model)

#remove outliers and set up a new model
if(!is_empty(outliers))
{
  print("Outliers detected; omitting overly influential cases and setting up new model")
  d_bigr.train <- d_bigr.train[-outliers, ]
  full.model <- glm(formula = formula, data = d_bigr.train, family = binomial)
  best.model <- full.model %>% MASS::stepAIC(direction = "both")
}
rm(outliers)

#there are no numeric variables in the model, so no assumptions can be checked

#calculate coefficients and save them to a data frame
coefs <- getCoefs(best.model, varName = "bigramm_next", fileName = "bigrams")

#plot the fifteen highest and lowest coefs
max_coefs <- head(arrange(coefs, desc(coefs)),20)
min_coefs <- tail(arrange(coefs, desc(coefs)),20)
coefs_ext <- rbind(max_coefs, min_coefs)
coefs_ext <- mutate(coefs_ext, pos = ifelse(coefs > 0, coefs + 0.7, coefs - 0.7))
coefs_ext <- arrange(coefs_ext, desc(coefs))
plot_coefs(coefs_ext, name = "bigrams")

#evaluate model
summary(best.model)
crossvalidate(best.model, d_bigr.test)
descr::LogRegR2(best.model)

#clean up
rm(coefs_ext, d_bigr, d_bigr.test, d_bigr.train, full.model, max_coefs, min_coefs, best.model, coefs, formula)
