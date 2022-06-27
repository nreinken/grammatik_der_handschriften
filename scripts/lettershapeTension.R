#lettershapeTension.R
#checks if stressed and unstressed vowels have different letter shapes
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
d <- dplyr::select(d, letter_rec, code_neu, phon_vtension, phon_class)
str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$phon_class <- as.factor(d$phon_class)
d$phon_vtension <- as.factor(d$phon_vtension)

#remove upper case and unrecognizable letters and keep only full vowels
d <- filter(d, phon_class == "VFULL")
d$phon_class <- NULL
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))
d <- droplevels(d) 

summary(d)


####a####
#correlation of a shapes and vowel stress 
d_a <- droplevels(filter(d, letter_rec == "a"))

(t_a <- table(d_a$code_neu, d_a$phon_vtension))

#chisq.test
(test <- chisq.test(t_a))  ##no difference
assocstats(t_a)
chisq.posthoc.test(t_a)

#save plot
png("assocplot_a_tension.png")
assocplot(t_a)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("aGespanntheit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####e####
#correlation of e shapes and vowel stress  
d_e <- droplevels(filter(d, letter_rec == "e"))

(t_e <- table(d_e$code_neu, d_e$phon_vtension))

(test <- chisq.test(t_e))  ##no difference
assocstats(t_e)
chisq.posthoc.test(t_e)

#save plot
png("assocplot_e_tension.png")
assocplot(t_e)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("eGespanntheit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####i####
#i has only one shape
####o####
#correlation of o shapes and vowel stress  
d_o <- droplevels(filter(d, letter_rec == "o"))

(t_o <- table(d_o$code_neu, d_o$phon_vtension))

(test <- chisq.test(t_o))  ##no difference
assocstats(t_o)
chisq.posthoc.test(t_o)

#save plot
png("assocplot_o_tension.png")
assocplot(t_o)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("oGespanntheit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####u####
#correlation of u shapes and vowel stress  
d_u <- droplevels(filter(d, letter_rec == "u"))

(t_u <- table(d_u$code_neu, d_u$phon_vtension))

(test <- chisq.test(t_u))  ##no difference
assocstats(t_u)
chisq.posthoc.test(t_u)

#save plot
png("assocplot_u_tension.png")
assocplot(t_u)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("uGespanntheit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####y####
#correlation of y shapes and vowel stress  
d_y <- droplevels(filter(d, letter_rec == "y"))

(t_y <- table(d_y$code_neu, d_y$phon_vtension))

(test <- chisq.test(t_y))  ##no difference
assocstats(t_y)
chisq.posthoc.test(t_y)

#save plot
png("assocplot_y_tension.png")
assocplot(t_y)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("yGespanntheit", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

