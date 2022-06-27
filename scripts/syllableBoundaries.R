#syllableBoundaries.R
#checks if letters at syllable boundaries are typically joined or not
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

#remove unnecessary columns
d <- dplyr::select(d, junc_border, gsyll_border, psyll_border, WaZ, word_struc)

#factorize
d$junc_border <- as.factor(d$junc_border)
d$gsyll_border <- as.factor(d$gsyll_border)
d$psyll_border <- as.factor(d$psyll_border)
d$word_struc <- as.factor(d$word_struc)
d$WaZ <- as.logical(d$WaZ)
str(d)

#remove line break separation and last letters
d <- filter(d, !WaZ)
d <- filter(d, word_struc != "fin")
d$WaZ <- NULL
d$word_struc <- NULL
d <- droplevels(d) 
summary(d)


####phonological syllable boundaries####

d_psyll <- d

#contingency test
(t_psyll <- table(d_psyll$junc_border, d_psyll$psyll_border))
(test <- chisq.test(t_psyll))  ##significant
assocstats(t_psyll)
chisq.posthoc.test(t_psyll)

#save plot
png("assocplot_juncBorder_psyll.png")
assocplot(t_psyll)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungPhonographischeGrenze", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####graphematical syllable boundaries####

d_gsyll <- d

#contingeny test
(t_gsyll <- table(d_gsyll$junc_border, d_gsyll$gsyll_border))
(test <- chisq.test(t_gsyll))  ##significant
assocstats(t_gsyll)
chisq.posthoc.test(t_gsyll)

#save plot
png("assocplot_juncBorder_gsyll.png")
assocplot(t_gsyll)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungGraphematischeGrenze", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")
