#lettershapesPhon.R
#determine if handwritten letterforms and phonological classes of the corresponding sound are dependent
#© Niklas Reinken, July 2021


options(scipen = 999)

#load libraries
library(tidyverse)
library(janitor)
library(beepr)
library(nnet)
library(caret)
library(colorspace)

#read corpus data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d, c("cols", "rows"))

#select only necessary columns
d <- dplyr::select(d, code_neu, letter_rec, phon_vred_type, phon_vposition, 
                   phon_vopen, phon_vtension, phon_ctype, phon_cloc,
                   phon_cvoiced, kopf_form, koda1_form, koda2_form)

d <- data.frame(lapply(d, factor))
str(d)

#remove upper-case and unrecognizable letters
d <- droplevels(filter(d, letter_rec != "0"))
d <- droplevels(filter(d, !str_detect(code_neu, "99")))

d$code_neu <- NULL
d$letter_rec <- NULL

summary(d)
lapply(d, table)
gc()

####head shape####

#create model
set.seed(8)
training.sample <- d$kopf_form %>%
  createDataPartition(p = 0.8, list = F)
train.data <- d[training.sample, ]
test.data <- d[-training.sample, ]

#!This may take a long time! Do not run if not necessary!
model <- nnet::multinom(data = train.data, kopf_form ~ phon_vred_type + phon_vposition + phon_vopen 
              + phon_vtension + phon_ctype + phon_cloc + phon_cvoiced)
beep()

predicted.classes <- model %>% predict(test.data)
mean(predicted.classes == test.data$kopf_form)

model
tt <- broom::tidy(model)
saveRDS(tt, "tidyCoefs.rds")
#tt <- readRDS("tidyCoefs.rds")
tt$sig <- ifelse(tt$p.value < 0.05, "sig", "")
tt$p.value <- round(tt$p.value, 3)
tt <- filter(tt, sig =="sig")
tt
beep()

#save coefs to .csv
write_csv2(tt, "coefs_kopfform.csv")


#prepare data for visual depiction by translating/renaming variables and removing unnecessary values
tt <- filter(tt, term != "(Intercept)")
tt <- filter(tt, !str_detect(term, "n.V."))
tt <- filter(tt, !str_detect(y.level, "n.V."))

replace.x = c("phon_vpositionFRONT" = "vorderer Vokal",
            "phon_vtensionTENSE" = "gespannter Vokal",
            "phon_vtensionNTENSE" = "ungespannter Vokal",
            "phon_vred_typeTIEF" = "Tiefschwa",
            "phon_vred_typeZENTR" = "Zentralschwa",
            "phon_vopenOPEN" = "offener Vokal",
            "phon_vopenMID" = "halboffener Vokal",
            "phon_cvoicedSTL" = "stimmloser Konsonant",
            "phon_ctypePLOS" = "Plosiv",
            "phon_ctypeORAL" = "Liquid/Sonorant",
            "phon_ctypeNASAL" = "Nasal",
            "phon_clocKORO" = "koronaler Konsonant",
            "phon_clocGLOTT" = "glottaler Konsonant",
            "phon_clocDORSA" = "dorsaler Konsonant")
tt$term <- plyr::revalue(tt$term, replace.x)
tt$term <- factor(tt$term, c("Zentralschwa", "Tiefschwa", "vorderer Vokal", "offener Vokal", "halboffener Vokal", "gespannter Vokal", "ungespannter Vokal", "Plosiv", "Nasal", "Liquid/Sonorant", "koronaler Konsonant", "dorsaler Konsonant", "glottaler Konsonant", "stimmloser Konsonant"))

replace.y = c("STRICH VERT KURZ" = "kurzer Strich",
              "STRICH VERT LANG" = "langer Strich",
              "SCHLAUFE KURZ" = "kurze Schlaufe",
              "SCHLAUFE LANG" = "lange Schlaufe",
              "GERUNDET LINKS OBEN" = "links oben gerundet",
              "GERUNDET RECHTS OBEN" = "rechts oben gerundet",
              "PUNKT" = "Punkt",
              "SCHRÄG RAUF" = "schräg rauf",
              "SCHRÄG RUNTER" = "schräg runter")
tt$y.level<- plyr::revalue(tt$y.level, replace.y)
tt$y.level <- factor(tt$y.level, c("kurzer Strich", "langer Strich", "kurze Schlaufe", "lange Schlaufe", "schräg rauf", "schräg runter", "links oben gerundet", "rechts oben gerundet", "Punkt"))


#plot
ggplot(tt, aes(fill = estimate, y = term, x = y.level)) +
  geom_raster() +
  theme_minimal() +
  rotate_x_text(45) +
  xlab("")  +
  ylab("") +
  scale_fill_continuous_diverging(palette = "Blue-Red3")
ggsave("Kopfform_Lautklasse.png")


#prepare for heatmap
tts <- spread(tt, key = term, value = estimate)
tts$std.error <- NULL
tts$statistic <- NULL
tts$p.value <- NULL
tts$sig <- NULL


tts <- group_by(tts, y.level) %>%
  summarize_all(~first(na.omit(.)))

tts <- as.data.frame(tts)
rownames(tts) <- tts$y.level
tts <- tts[,-1]

for(i in 1:ncol(tts))
{
 tts[,i] <- ifelse(is.na(tts[,i]), 0, tts[,i]) 
}

#draw heatmap
tts <- data.matrix(tts)
heatmap(tts)

####coda shapes####
#create model
set.seed(8)
training.sample <- d$koda1_form %>%
  createDataPartition(p = 0.8, list = F)
train.data <- d[training.sample, ]
test.data <- d[-training.sample, ]

model <- multinom(data = train.data, koda1_form ~ phon_vred_type + phon_vposition + phon_vopen 
                  + phon_vtension + phon_ctype + phon_cloc + phon_cvoiced)
beep()

predicted.classes <- model %>% predict(test.data)
mean(predicted.classes == test.data$koda1_form)

model
tt <- broom::tidy(model)
saveRDS(tt, "tidyCoefs_koda1.rds")
#tt <- readRDS("tidyCoefs_koda1.rds")
tt$sig <- ifelse(tt$p.value < 0.05, "sig", "")
tt$p.value <- round(tt$p.value, 3)
tt <- filter(tt, sig =="sig")
tt
beep()

#write coefs to .csv
write_csv2(tt, "coefs_kodaform.csv")


#prepare for visualization by renaming and removing
tt <- filter(tt, term != "(Intercept)")
tt <- filter(tt, !str_detect(term, "n.V."))
tt <- filter(tt, !str_detect(y.level, "n.V."))

replace.x = c("phon_vpositionFRONT" = "vorderer Vokal",
              "phon_vtensionTENSE" = "gespannter Vokal",
              "phon_vtensionNTENSE" = "ungespannter Vokal",
              "phon_vred_typeTIEF" = "Tiefschwa",
              "phon_vred_typeZENTR" = "Zentralschwa",
              "phon_vopenOPEN" = "offener Vokal",
              "phon_vopenMID" = "halboffener Vokal",
              "phon_cvoicedSTL" = "stimmloser Konsonant",
              "phon_cvoicedSTH" = "stimmhafter Konsonant",
              "phon_ctypePLOS" = "Plosiv",
              "phon_ctypeORAL" = "Liquid/Sonorant",
              "phon_ctypeNASAL" = "Nasal",
              "phon_clocKORO" = "koronaler Konsonant",
              "phon_clocGLOTT" = "glottaler Konsonant",
              "phon_clocLAMI" = "laminaler Konsonant",
              "phon_clocLABI" = "labialer Konsonant",
              "phon_clocDORSA" = "dorsaler Konsonant")
tt$term <- plyr::revalue(tt$term, replace.x)
tt$term <- factor(tt$term, c("Zentralschwa", "Tiefschwa", "vorderer Vokal", "offener Vokal", "halboffener Vokal", "gespannter Vokal", "ungespannter Vokal", "Plosiv", "Nasal", "Liquid/Sonorant", "labialer Konsonant", "laminaler Konsonant", "koronaler Konsonant", "dorsaler Konsonant", "glottaler Konsonant", "stimmloser Konsonant", "stimmhafter Konsonant"))

replace.y = c("BOGEN OBEN" = "Bogen nach oben",
              "BOGEN RECHTS OFFEN" = "Bogen nach links",
              "BOGEN UNTEN" = "Bogen nach unten",
              "GERUNDET RECHTS UNTEN" = "rechts unten gerundet",
              "GERUNDET LINKS OBEN" = "links oben gerundet",
              "STRICH HORI" = "horizontaler Strich",
              "SCHRÄG RAUF" = "schräg rauf",
              "SCHRÄG RUNTER" = "schräg runter")
tt$y.level<- plyr::revalue(tt$y.level, replace.y)
tt$y.level <- factor(tt$y.level, c("horizontaler Strich","schräg rauf", "schräg runter", "Bogen nach links", "Bogen nach rechts", "Bogen nach unten", "Bogen nach oben", "links oben gerundet", "rechts unten gerundet"))


#plot
ggplot(tt, aes(fill = estimate, y = term, x = y.level)) +
  geom_raster() +
  theme_minimal() +
  rotate_x_text(45) +
  xlab("")  +
  ylab("") +
  scale_fill_continuous_diverging(palette = "Blue-Red3")
ggsave("koda1_form_Lautklasse.png")
