#junctionsLetters.R
#tests the junction frequency for each letter and each letter shape
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(janitor)
library(beepr)
library(speedglm)
library(tidyverse)
library(ggrepel)

#load data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d)

#select only necessary columns
d <- dplyr::select(d, code_neu, junc_border, WaZ, word_struc, next_letter, prev_letter, letter_rec)
str(d)

#factorize
d$word_struc <- as.factor(d$word_struc)
d$next_letter <- as.factor(d$next_letter)
d$prev_letter <- as.factor(d$prev_letter)
d$code_neu <- as.factor(d$code_neu)

#remove upper case letters, last letters and line break separations
d <- filter(d, d$WaZ != T)
d <- filter(d, d$word_struc != "fin")
d <- filter(d, !d$prev_letter %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", 
                                 "J", "K", "L", "M", "N", "O", "P", "Q", "R", 
                                 "S", "T", "U", "V", "W", "X", "Y", "Z", "Ä", "Ö", "Ü"))
d <- droplevels(d)
glimpse(d)
summary(d)

####junction and letters####
#set up model
set.seed(8)
full.model <- speedglm(formula = junc_border ~ next_letter + prev_letter,
                       data = d, family = binomial())
#full.model <- readRDS("model_VerbundenheitBigramme.rds")
saveRDS(full.model, "model_VerbundenheitBuchstaben.rds")
beep()

#calculate coefficients
coefs <- round(full.model$coefficients,3)
coefs <- data.frame(coefs)
coefs <- cbind(rownames(coefs), data.frame(coefs, row.names=NULL))
coefs$`rownames(coefs)` <- str_replace_all(coefs$`rownames(coefs)`, "next_letter", "")

#select significant variables
toselect.x <- summary(full.model)$coeff[-1,4] < 0.05
coefs <- coefs[toselect.x == TRUE,]
coefs <- na.omit(coefs)
#save coefficients to .csv
write.csv2(coefs, "koeffizienten_VerbundenheitBuchstaben.csv")

summary(full.model)

# Make predictions
probabilities <- full.model %>% predict(d, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, T, F)

# Model accuracy
mean(predicted.classes == d$junc_border) #about 62.4 % of cases are predicted correctly

#prepare coefficients for plotting
coefs_ext <- arrange(coefs, desc(coefs))
coefs_ext <- mutate(coefs_ext, color = ifelse(str_detect(`rownames(coefs)`,"prev_letter"), "vorheriger Buchstabe", "nachfolgender Buchstabe"))
coefs_ext$`rownames(coefs)` <- str_remove_all(coefs_ext$`rownames(coefs)`,"prev_letter")
coefs_ext$`rownames(coefs)` <- str_remove_all(coefs_ext$`rownames(coefs)`,"next_letter")
coefs_ext <- mutate(coefs_ext, name = `rownames(coefs)`)
coefs_ext <- mutate(coefs_ext, pos = ifelse(coefs > 0, coefs + 0.5, coefs - 0.5))
coefs_ext <- arrange(coefs_ext, desc(coefs))

#plot coefficients
ggplot(data = droplevels(filter(coefs_ext, color == "nachfolgender Buchstabe")), aes(x = reorder(name, coefs), y = coefs)) +
  geom_point(stat = "identity", size = 5, shape = 16) +
  geom_segment(aes(y = 0, x = name, yend = coefs, xend = name)) +
  geom_text(aes(y = pos, label = str_c(name,": ", coefs)) ,color = "#222222", size = 3) +
  geom_segment(aes(x = length(name)+1, y = 0, yend = 0, xend = 0), color = "darkred") +
  ylim(-2, 2) +
  coord_flip() +
  labs(y = "Koeffizienten", x = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank())
ggsave("Koeffizienten_nachfolgenderBuchstabe.png", width = 4, height = 6)

#plot coefficients
ggplot(data = droplevels(filter(coefs_ext, color == "vorheriger Buchstabe")), aes(x = reorder(name, coefs), y = coefs)) +
  geom_point(stat = "identity", size = 5, shape = 16) +
  geom_segment(aes(y = 0, x = name, yend = coefs, xend = name)) +
  geom_text(aes(y = pos, label = str_c(name,": ", coefs)) ,color = "#222222", size = 3) +
  geom_segment(aes(x = length(name)+1, y = 0, yend = 0, xend = 0), color = "darkred") +
  ylim(-2, 2) +
  coord_flip() +
  labs(y = "Koeffizienten", x = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank())
ggsave("Koeffizienten_vorherigerBuchstabe.png", width = 4, height = 6)

#assumptions

#linearity
#no numeric variables

#influential data points
#no numeric variables

#multicollinearity
#no numeric variables



####junction and letter shapes####

#exclude unrecognizable letters
extr.x <- str_detect(d$code_neu, "99")
d_neu <- d[extr.x == F,]

#set up model
set.seed(8)
form.model <- speedglm(formula = junc_border ~ code_neu,
                       data = d_neu, family = binomial())
#form.model <- readRDS("model_VerbundenheitForm.rds")
saveRDS(full.model, "model_VerbundenheitForm.rds")
beep()

#calculate coefficients
coefs <- round(form.model$coefficients,3)
coefs <- data.frame(coefs)
coefs <- cbind(rownames(coefs), data.frame(coefs, row.names=NULL))
coefs$`rownames(coefs)` <- str_replace_all(coefs$`rownames(coefs)`, "code_neu", "")

#select significant variables
toselect.x <- summary(form.model)$coeff[-1,4] < 0.05
coefs <- coefs[toselect.x == TRUE,]
coefs <- na.omit(coefs)
#save coefs to .csv
write.csv2(coefs, "koeffizienten_VerbundenheitForm.csv")

summary(form.model)


# Make predictions
probabilities <- form.model %>% predict(d, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, T, F)

# Model accuracy
mean(predicted.classes == d$junc_border) #about 66.3 % of cases are predicted correctly

#prepare coefs for plotting
coefs_ext <- arrange(coefs, desc(coefs))
coefs_ext$`rownames(coefs)` <- str_remove_all(coefs_ext$`rownames(coefs)`,"code_neu")
coefs_ext <- mutate(coefs_ext, name = `rownames(coefs)`)
coefs_ext <- mutate(coefs_ext, pos = ifelse(coefs > 0, coefs + 1.5, coefs - 1.5))
coefs_ext <- arrange(coefs_ext, coefs)
coefs_ext$`rownames(coefs)` <- NULL
max_coefs <- head(arrange(coefs_ext, desc(coefs_ext)),15)
min_coefs <- tail(arrange(coefs_ext, desc(coefs_ext)),15)
coefs_ext <- rbind(max_coefs, min_coefs)

#plot
ggplot(data = coefs_ext, aes(x = reorder(name, coefs), y = coefs)) +
  geom_point(stat = "identity", size = 5, fill = "white", color ="#222222", shape = 16) +
  geom_segment(aes(y = 0, x = name, yend = coefs, xend = name), color = "#222222") +
  geom_text(aes(y = pos, label = str_c(name,": ", coefs)) ,color = "#222222", size = 3) +
  geom_segment(aes(x = length(name)+1, y = 0, yend = 0, xend = 0), color = "darkred") +
  coord_flip() +
  labs(y = "Koeffizienten", x = "") +
  theme_minimal()+
  theme(axis.text.y = element_blank()) +
  ylim(-5.2,5.2)
ggsave("Koeffizienten_Buchstabenform.png", width = 4, height = 8)



#assumptions

#linearity
#no numeric variables

#influential data points
#no numeric variables

#multicollinearity
#no numeric variables

