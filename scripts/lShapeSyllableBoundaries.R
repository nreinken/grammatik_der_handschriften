#lShapeSyllableBoundaries.R
#checks if the l form corresponds with syllable borders
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
d <- dplyr::select(d, letter_rec, code_neu, gsyll_border, psyll_border)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$gsyll_border <- as.factor(d$gsyll_border)
d$psyll_border <- as.factor(d$psyll_border)
d$code_neu <- as.factor(d$code_neu)
str(d)
summary(d)

#since graphematic and phonological syllable boundaries might differ, I will only look at the cases in which they match
d$silbengrenze <- ifelse(d$gsyll_border == d$psyll_border, "gleich","ungleich")
d <- filter(d, silbengrenze == "gleich")
d$silbengrenze <- d$gsyll_border
d$psyll_border <- NULL
d$gsyll_border <- NULL
d <- droplevels(d)

#remove every letter but l, remove unrecognizable letters
d <- droplevels(filter(d, letter_rec == "l"))
d <- droplevels(filter(d, code_neu != "l99"))


#contingency test for l shape and syllable boundary
(t_l <- table(d$code_neu, d$silbengrenze))
(test <- chisq.test(t_l))  ##significant
assocstats(t_l)
chisq.posthoc.test(t_l)

#save plot
png("assocplot_l_Silbengrenze.png")
assocplot(t_l)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("lFormSilbengrenze", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")
