#lettershapeVoicedness.R
#checks if the letter shape is different with voiced and unvoiced consonants
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#select only necessary columns
d <- dplyr::select(d, letter_rec, code_neu, phon_cvoiced, phon_class)
str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$phon_class <- as.factor(d$phon_class)
d$phon_cvoiced <- as.factor(d$phon_cvoiced)

#remove vowels, upper case und unrecognizable letters
d <- filter(d, phon_class == "CONS")
d$phon_class <- NULL
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))

#keep only consonants with a voicedness distinction
d <- filter(d, letter_rec %in% c("b", "d", "g", "s", "v"))
d <- droplevels(d) 
summary(d)


####b####
#shape of b and voicedness
d_b <- droplevels(filter(d, letter_rec == "b"))

#contingency
(t_b <- table(d_b$code_neu, d_b$phon_cvoiced))
(test <- chisq.test(t_b))  ##not significant
assocstats(t_b)
chisq.posthoc.test(t_b)

#plot
png("assocplot_b_voice.png")
assocplot(t_b)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("bStimmhaftigkeit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####d####
#shape of d and voicedness
d_d <- droplevels(filter(d, letter_rec == "d"))

#contingency
(t_d <- table(d_d$code_neu, d_d$phon_cvoiced))
(test <- chisq.test(t_d))  ##beim d passiert was
assocstats(t_d)
chisq.posthoc.test(t_d)

#plot
png("assocplot_d_voice.png", pointsize = 20)
assocplot(t_d)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("dStimmhaftigkeit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####g####
#shape of g and voicedness
d_g <- droplevels(filter(d, letter_rec == "g"))

#contingency
(t_g <- table(d_g$code_neu, d_g$phon_cvoiced))
(test <- chisq.test(t_g))  ##beim g passiert was
assocstats(t_g)
chisq.posthoc.test(t_g)

#plot
png("assocplot_g_voice.png", pointsize = 20)
assocplot(t_g)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("gStimmhaftigkeit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####s####
#shape of s and voicedness
d_s <- droplevels(filter(d, letter_rec == "s"))

#contingency
(t_s <- table(d_s$code_neu, d_s$phon_cvoiced))
(test <- chisq.test(t_s))  ##beim s passiert was
assocstats(t_s)
chisq.posthoc.test(t_s)

#plot
png("assocplot_s_voice.png", pointsize = 20)
assocplot(t_s)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("sStimmhaftigkeit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")
