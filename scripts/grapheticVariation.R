#grapheticVariation.R
#calculates the overall syntagmatic and paradigmatic variation based on regression models
#© Niklas Reinken, July 2021
options(scipen = 0)

#load libraries
library(janitor)
library(beepr)
library(descr)
library(car)
library(caret)
library(broom)
library(nnet)
library(MASS)
library(tidyverse)

#load data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d)

#select only necessary columns
d <- dplyr::select(d, person_ID, word_index, junc_border, letter, letter_freq, WaZ, word_struc, code_neu)
str(d)

#factorize
d$person_ID <- as.factor(d$person_ID)
d$word_struc <- as.factor(d$word_struc)
d$code_neu <- as.factor(d$code_neu)

#recode umlaute as base letters
d$letter <- plyr::revalue(d$letter, c("ä" = "a", "ö" = "o", "ü" = "u"))
d <- droplevels(d)
glimpse(d)
summary(d)


####syntagmatic variation####

#create data subset, keep only necessary columns, remove line break separation and last letters
d_syn <- filter(d, d$WaZ != T)
d_syn <- filter(d_syn, d_syn$word_struc != "fin")
d_syn <- dplyr::select(d_syn, junc_border, person_ID, word_index, letter_freq)


#log numeric variables
d_syn$word_index <- log(d_syn$word_index)
d_syn$letter_freq <- log(d_syn$letter_freq)

# Split the data into training and test set
set.seed(8)
training.samples <- d_syn$junc_border %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- d_syn[training.samples, ]
test.data <- d_syn[-training.samples, ]

#set up the model
#DO NOT RUN! TAKES A LOT OF TIME!
full.model <- glm(junc_border ~ person_ID + word_index + letter_freq,
                  data = train.data, family = binomial)
beep()
#DO NOT RUN! TAKES A LOT OF TIME!
step.model <- full.model %>% stepAIC(direction = "both")
#DO NOT RUN! TAKES A LOT OF TIME!
coef(step.model)
beep()

summary(step.model)

# Make predictions
probabilities <- step.model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, T, F)

# Model accuracy
mean(predicted.classes == test.data$junc_border) #about 66.7% of cases are predicted correctlyig vorhergesagt

#DO NOT RUN! TAKES A LOT OF TIME!
LogRegR2(step.model)

#check assumptions
probabilities <- step.model %>% predict(d_syn, type = "response")

#check linearity
mydata <- d_syn %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)

#plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~predictors, scales = "free_y") +
  theme_bw()
beep()
ggsave("model_externFactors_assump_linearity.png")
beep()

#check for overly influential data points
model.data <- augment(step.model) %>%
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) +
  geom_point(aes(color = junc_border), alpha = 0.5) +
  theme_bw()
ggsave("model_externFactors_assump_influence.png")
model.data %>%
  filter(abs(.std.resid) > 3) #only one data point is suspicious

#check for multicollinearity
car::vif(step.model) #no collinearity

#####paradigmatic variation####
#create subset with only necessary columns and remove upper-case letters
d_par <- dplyr::select(d, code_neu, letter, person_ID, word_index, letter_freq)
d_par <- filter(d_par, d_par$code_neu != "0")
d_par <- droplevels(d_par)

#create a model for each letter (one model for all letters would have cases with to few occurences)
#DO NOT RUN! TAKES A LOT OF TIME!
paraModel <- function(character, data)
{
  data <- filter(data, data$letter == character)
  data <- droplevels(data)
  
  #create train and test data
  training.samples <- data$code_neu %>%
    createDataPartition(p = 0.8, list = F)
  train.data <- data[training.samples,]
  test.data <- data[-training.samples,]
  
  #set up model
  model <- nnet::multinom(code_neu ~ person_ID + word_index + letter_freq,
                          data = train.data,
                          MaxNWts = 9999)
  
  predicted.classes <- model %>% predict(test.data)
  return(mean(predicted.classes == test.data$code_neu))
}

#combine all letters into a data frame
letters <- sort(unique(d_par$letter))
predictRates <- data.frame()
colnames(predictRates) <- c("letter", "predictRate")

#run function for paradigmatic variation models for each letter in list
for(char in letters)
{
 print(char)
  rate <- paraModel(char, data = d_par)
 #add prediction rate to data frame
  predictRates <- rbind(predictRates, c("letter" = char, "predictRate" = rate))
}
predictRates

