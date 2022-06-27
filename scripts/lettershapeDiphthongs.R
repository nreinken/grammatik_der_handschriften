#lettershapeDiphthongs.R
#determine if handwritten lettershapes are different in diphthongs
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
d <- dplyr::select(d, letter_rec, code_neu, phon_class, gsyll_struc, phon_complexity)

str(d)
#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$phon_class <- as.factor(d$phon_class)
d$gsyll_struc <- as.factor(d$gsyll_struc)
d$phon_complexity <- as.factor(d$phon_complexity)

#filter non-vowels, upper-case and unrecognizable letters
d <- filter(d, phon_class %in% c("VFULL", "n.V."))
d$phon_class <- NULL
d <- filter(d, letter_rec %in% c("a", "e", "i", "o", "u", "y"))
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))
d <- droplevels(d) 

summary(d)


####shapes of a in key-position (i.e. second part of diphthong)####
#use only a's
d_a <- droplevels(filter(d, letter_rec == "a"))

(t_a <- table(d_a$code_neu, d_a$gsyll_struc))

#fisher-test
(test <- fisher.test(t_a))  ##no difference in shapes
vcd::assocstats(t_a)
chisq.posthoc.test(t_a)

#plot
png("assocplot_a_key.png")
assocplot(t_a)
dev.off()

#store p-value externally to apply bonferroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("aKey", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####shapes of e in key-position (i.e. second part of diphthong)####
#use only e's
d_e <- droplevels(filter(d, letter_rec == "e"))

(t_e <- table(d_e$code_neu, d_e$gsyll_struc))

#chisq-test
(test <- chisq.test(t_e))  #there's difference in shapes
vcd::assocstats(t_e)
chisq.posthoc.test(t_e)

#plot
png("assocplot_e_key.png")
assocplot(t_e)
dev.off()

#store p-value externally to apply bonferroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("eKey", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####shapes of e in key-position (i.e. second part of diphthong)####
d_u <- droplevels(filter(d, letter_rec == "u"))

(t_u <- table(d_u$code_neu, d_u$gsyll_struc))

#chisq-test
(test <- chisq.test(t_u))  ##there is a difference, but it probably won't hold against the holm correction
vcd::assocstats(t_u)
chisq.posthoc.test(t_u)

#plot
png("assocplot_u_key.png")
assocplot(t_u)
dev.off()

#store p-value externally to apply bonferroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("uKey", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


