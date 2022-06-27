#junctionsBigrams.R
#tests the junction frequency for each bigram with more than 100 occurences
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(janitor)
library(beepr)
library(caret)
library(broom)
library(speedglm)
library(tidyverse)
library(jtools)
library(ggstance)

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data)

#select only necessary columns
d <- dplyr::select(d, junc_border, WaZ, word_struc, bigramm_next, letter)
str(d)

#factorize
d$word_struc <- as.factor(d$word_struc)
d$bigramm_next <- as.factor(d$bigramm_next)

#remove last letters, line break separations and upper case letters
d <- filter(d, d$WaZ != T)
d <- filter(d, d$word_struc != "fin")
d <- filter(d, !d$letter %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", 
                                     "J", "K", "L", "M", "N", "O", "P", "Q", "R", 
                                     "S", "T", "U", "V", "W", "X", "Y", "Z", "Ä", "Ö", "Ü"))
d <- droplevels(d)
glimpse(d)
summary(d)


#filter bigrams with a low frequency, below 100
freqs <- data.frame(table(d$bigramm_next))
low_freqs <- filter(freqs, Freq < 100)
d <- filter(d, !bigramm_next %in% low_freqs$Var1)
d <- droplevels(d)

#create model
#DO NOT RUN! TAKES A LONG TIME!
set.seed(8)
full.model <- speedglm(formula = junc_border ~ bigramm_next,
                  data = d, family = binomial())
#full.model <- readRDS("model_VerbundenheitBigramme.rds")
saveRDS(full.model, "model_VerbundenheitBigramme.rds")
beep()

#calculate coefficients and save them to a data frame
coefs <- round(full.model$coefficients,2)
coefs <- data.frame(coefs)
coefs <- cbind(rownames(coefs), data.frame(coefs, row.names=NULL))
coefs$`rownames(coefs)` <- str_replace_all(coefs$`rownames(coefs)`, "bigramm_next", "")

#select significant variables
toselect.x <- summary(full.model)$coeff[-1,4] < 0.05
coefs <- coefs[toselect.x == TRUE,]
coefs <- na.omit(coefs)
#save to a .csv
write.csv2(coefs, "koeffizienten_VerbundenheitBigramme.csv")

summary(full.model)


# Make predictions
probabilities <- full.model %>% predict(d, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, T, F)

# Model accuracy
mean(predicted.classes == d$junc_border) #about 69.1 % of cases are predicted correctly

#plot the fifteen highest and lowest coefs
max_coefs <- head(arrange(coefs, desc(coefs)),20)
min_coefs <- tail(arrange(coefs, desc(coefs)),20)
coefs_ext <- rbind(max_coefs, min_coefs)
coefs_ext <- mutate(coefs_ext, pos = ifelse(coefs > 0, coefs + 0.7, coefs - 0.7))
coefs_ext <- arrange(coefs_ext, desc(coefs))

ggplot(data = coefs_ext, aes(x = reorder(`rownames(coefs)`, coefs), y = coefs)) +
  geom_point(stat = "identity", size = 5, fill = "white", color ="#222222", shape = 16) +
  geom_segment(aes(y = 0, x = `rownames(coefs)`, yend = coefs, xend = `rownames(coefs)`), color = "#222222") +
  geom_text(aes(y = pos, label = str_c(`rownames(coefs)`,": ", coefs)) ,color = "#222222", size = 3) +
  geom_segment(aes(x = length(`rownames(coefs)`)+1, y = 0, yend = 0, xend = 0), color = "darkred") +
  coord_flip() +
  labs(y = "Koeffizienten", x = "") +
  theme_minimal()+
  theme(axis.text.y = element_blank()) +
  ylim(-3,3)
ggsave("Koeffizienten_Bigramme.png", width = 4, height = 8)


#assumptions

#linearity
#no numeric variables

#influential data points
#no numeric variables

#multicollinearity
#no numeric variables
