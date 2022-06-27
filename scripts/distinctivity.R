#distinctivity.R
#determine if similar basic shapes occur at the same positions within a syllable
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

#select only necessary colums
d <- dplyr::select(d, letter_rec, code_neu, gsyll_struc)

str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$gsyll_struc <- as.factor(d$gsyll_struc)
d$gsyll_struc <- factor(d$gsyll_struc, levels = c("ONS", "NUC", "KEY", "CODA", "EXTRA"))

#omit upper case and unrecognizable letters
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))
d <- droplevels(d) 

summary(d)

####c1 and e3####
#difference between c und e in the Key-position 
#(c has only one shape, so we'll be looking at e)
d_ce <- droplevels(filter(d, letter_rec == "e"))

(t_ce <- table(d_ce$code_neu, d_ce$gsyll_struc))

#chisq.test
(test <- chisq.test(t_ce)) #there is a significant difference
assocstats(t_ce)
chisq.posthoc.test(t_ce)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("eForm_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####f6 and t1####
d_ft <- droplevels(filter(d, letter_rec %in% c("f", "t")))

#we only compare f6 to t1, so I'll recode every other shape to "other shape" 
new.values <- c("f1" = "anderes <f>",
                "f2" = "anderes <f>",
                "f3" = "anderes <f>",
                "f4" = "anderes <f>",
                "f5" = "anderes <f>",
                "f7" = "anderes <f>",
                "f9" = "anderes <f>",
                "f8" = "anderes <f>",
                "t2" = "anderes <t>",
                "t3" = "anderes <t>",
                "t4" = "anderes <t>")
d_ft$code_neu <- plyr::revalue(d_ft$code_neu, new.values)
d_ft <- droplevels(d_ft)
d_ft$code_neu <- factor(d_ft$code_neu, levels = c("anderes <f>", "f6", "anderes <t>", "t1"))


d_f <- droplevels(filter(d_ft, letter_rec %in% c("f")))
d_t <- droplevels(filter(d_ft, letter_rec %in% c("t")))


#f
(t_f <- table(d_f$code_neu, d_f$gsyll_struc))

#fisher-test
(test <- fisher.test(t_f)) #there is a difference, but it probably won't survive holm
assocstats(t_f)
chisq.posthoc.test(t_f)

png("assocplot_f6_distinktAufPos.png")
assocplot(t_f)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("f6-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#t
(t_t <- table(d_t$code_neu, d_t$gsyll_struc))

#chisq.test
(test <- chisq.test(t_t)) #there is a difference
assocstats(t_t)
chisq.posthoc.test(t_t)

png("assocplot_t1_distinktAufPos.png")
assocplot(t_t)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("t1-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####f7, h5, l1, t3####
d_fhlt <- droplevels(filter(d, letter_rec %in% c("f", "h", "l", "t")))
#recode other instances of the letters to "other"
new.values <- c("f1" = "anderes <f>",
                "f2" = "anderes <f>",
                "f3" = "anderes <f>",
                "f4" = "anderes <f>",
                "f5" = "anderes <f>",
                "f6" = "anderes <f>",
                "f8" = "anderes <f>",
                "f9" = "anderes <f>",
                "h1" = "anderes <h>",
                "h2" = "anderes <h>",
                "h3" = "anderes <h>",
                "h4" = "anderes <h>",
                "h6" = "anderes <h>",
                "l2" = "anderes <l>",
                "t1" = "anderes <t>",
                "t2" = "anderes <t>",
                "t4" = "anderes <t>")
d_fhlt$code_neu <- plyr::revalue(d_fhlt$code_neu, new.values)
d_fhlt <- droplevels(d_fhlt)
d_fhlt$code_neu <- factor(d_fhlt$code_neu, levels = c("anderes <f>", "f7", "anderes <h>", "h5", "anderes <l>", "l1", "anderes <t>", "t3"))

d_f <- droplevels(filter(d_fhlt, letter_rec %in% c("f")))
d_h <- droplevels(filter(d_fhlt, letter_rec %in% c("h")))
d_l <- droplevels(filter(d_fhlt, letter_rec %in% c("l")))
d_t <- droplevels(filter(d_fhlt, letter_rec %in% c("t")))


#f
(t_f <- table(d_f$code_neu, d_f$gsyll_struc))

#fisher
(test <- fisher.test(t_f)) #there is a difference
assocstats(t_f)
chisq.posthoc.test(t_f)

png("assocplot_f7_distinktAufPos.png", pointsize = 20)
assocplot(t_f)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("f7-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#h
(t_h <- table(d_h$code_neu, d_h$gsyll_struc))

#fisher-test
(test <- fisher.test(t_h)) #there is a difference
assocstats(t_h)
chisq.posthoc.test(t_h)

png("assocplot_h5_distinktAufPos.png", pointsize = 20)
assocplot(t_h)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("h5-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


#l
(t_l <- table(d_l$code_neu, d_l$gsyll_struc))

#chisq.test
(test <- chisq.test(t_l)) #no difference
assocstats(t_l)
chisq.posthoc.test(t_l)

png("assocplot_l1_distinktAufPos.png", pointsize = 20)
assocplot(t_l)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("l1-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#t
(t_t <- table(d_t$code_neu, d_t$gsyll_struc))

#fisher
(test <- chisq.test(t_t)) #there is a difference
assocstats(t_t)
chisq.posthoc.test(t_t)

png("assocplot_t3_distinktAufPos.png", pointsize = 20)
assocplot(t_t)
dev.off()

##store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("t3-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####f9 and t2####
d_ft2 <- droplevels(filter(d, letter_rec %in% c("f", "t")))

#recode other instances of the letters to "other"
new.values <- c("f1" = "anderes <f>",
                "f2" = "anderes <f>",
                "f3" = "anderes <f>",
                "f4" = "anderes <f>",
                "f5" = "anderes <f>",
                "f7" = "anderes <f>",
                "f6" = "anderes <f>",
                "f8" = "anderes <f>",
                "t1" = "anderes <t>",
                "t3" = "anderes <t>",
                "t4" = "anderes <t>")
d_ft2$code_neu <- plyr::revalue(d_ft2$code_neu, new.values)
d_ft2 <- droplevels(d_ft2)
d_ft2$code_neu <- factor(d_ft2$code_neu, levels = c("anderes <f>", "f9", "anderes <t>", "t2"))


d_f <- droplevels(filter(d_ft2, letter_rec %in% c("f")))
d_t <- droplevels(filter(d_ft2, letter_rec %in% c("t")))

#f
(t_f <- table(d_f$code_neu, d_f$gsyll_struc))

#fisher
(test <- fisher.test(t_f)) #no difference
assocstats(t_f)
chisq.posthoc.test(t_f)

png("assocplot_f9_distinktAufPos.png")
assocplot(t_f)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("f9-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#t
(t_t <- table(d_t$code_neu, d_t$gsyll_struc))

#chisq.test
(test <- chisq.test(t_t)) #there is a difference, but it might not survive holm
assocstats(t_t)
chisq.posthoc.test(t_t)

png("assocplot_t2_distinktAufPos.png")
assocplot(t_t)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("t2-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####h4 and k7####
d_hk <- droplevels(filter(d, letter_rec %in% c("h", "k")))
#recode every other letter instance as "other"
new.values <- c("h1" = "anderes <h>",
                "h2" = "anderes <h>",
                "h3" = "anderes <h>",
                "h5" = "anderes <h>",
                "h6" = "anderes <h>",
                "k1" = "anderes <k>",
                "k2" = "anderes <k>",
                "k3" = "anderes <k>",
                "k4" = "anderes <k>",
                "k6" = "anderes <k>",
                "k5" = "anderes <k>",
                "k8" = "anderes <k>")
d_hk$code_neu <- plyr::revalue(d_hk$code_neu, new.values)
d_hk <- droplevels(d_hk)
d_hk$code_neu <- factor(d_hk$code_neu, levels = c("anderes <h>", "h4", "anderes <k>", "k7"))


d_h <- droplevels(filter(d_hk, letter_rec %in% c("h")))
d_k <- droplevels(filter(d_hk, letter_rec %in% c("k")))

#h
(t_h <- table(d_h$code_neu, d_h$gsyll_struc))

#chisq
(test <- chisq.test(t_h)) #no difference
assocstats(t_h)
chisq.posthoc.test(t_h)

png("assocplot_h4_distinktAufPos.png")
assocplot(t_h)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("h4-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#k
(t_k <- table(d_k$code_neu, d_k$gsyll_struc))

#chisq.test
(test <- chisq.test(t_k)) #no difference
assocstats(t_k)
chisq.posthoc.test(t_k)

png("assocplot_k7_distinktAufPos.png")
assocplot(t_k)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("k7-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####h6 und l2####
d_hl <- droplevels(filter(d, letter_rec %in% c("h", "l")))
#recode other instances of letters to "other"
new.values <- c("h1" = "anderes <h>",
                "h2" = "anderes <h>",
                "h3" = "anderes <h>",
                "h5" = "anderes <h>",
                "h4" = "anderes <h>",
                "l1" = "anderes <l>")
d_hl$code_neu <- plyr::revalue(d_hl$code_neu, new.values)
d_hl <- droplevels(d_hl)
d_hl$code_neu <- factor(d_hl$code_neu, levels = c("anderes <h>", "h6", "anderes <l>", "l2"))


d_h <- droplevels(filter(d_hl, letter_rec %in% c("h")))
d_l <- droplevels(filter(d_hl, letter_rec %in% c("l")))

#h
(t_h <- table(d_h$code_neu, d_h$gsyll_struc))

#fisher
(test <- fisher.test(t_h)) #there is a difference, but it probably won't survive holm
assocstats(t_h)
chisq.posthoc.test(t_h)

png("assocplot_h6_distinktAufPos.png")
assocplot(t_h)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("h6-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#l
(t_l <- table(d_l$code_neu, d_l$gsyll_struc))

#chisq
(test <- chisq.test(t_l)) #no difference
assocstats(t_l)
chisq.posthoc.test(t_l)

png("assocplot_l2_distinktAufPos.png")
assocplot(t_l)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("l2-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

####n2 und u2####
d_nu <- droplevels(filter(d, letter_rec %in% c("n", "u")))
#recode other letter shapes to "other"
new.values <- c("n1" = "anderes <n>",
                "u1" = "anderes <u>")
d_nu$code_neu <- plyr::revalue(d_nu$code_neu, new.values)
d_nu <- droplevels(d_nu)
d_nu$code_neu <- factor(d_nu$code_neu, levels = c("anderes <n>", "n2", "anderes <u>", "u2"))


d_n <- droplevels(filter(d_nu, letter_rec %in% c("n")))
d_u <- droplevels(filter(d_nu, letter_rec %in% c("u")))

#n
(t_n <- table(d_n$code_neu, d_n$gsyll_struc))

#chisq.test
(test <- chisq.test(t_n)) #no difference
assocstats(t_n)
chisq.posthoc.test(t_n)

png("assocplot_n2_distinktAufPos.png")
assocplot(t_n)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("n2-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#u
(t_u <- table(d_u$code_neu, d_u$gsyll_struc))

#chisq.test
(test <- chisq.test(t_u)) #there is a difference, but it won't survive holm
assocstats(t_l)
chisq.posthoc.test(t_l)

png("assocplot_l2_distinktAufPos.png")
assocplot(t_l)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("l2-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")




####r3 and v1####
d_r <- droplevels(filter(d, letter_rec %in% c("r")))
#recode other instances of r to 'other'
new.values <- c("r1" = "anderes <r>",
                "r2" = "anderes <r>",
                "r4" = "anderes <r>",
                "r5" = "anderes <r>")
d_r$code_neu <- plyr::revalue(d_r$code_neu, new.values)
d_r <- droplevels(d_r)
d_r$code_neu <- factor(d_r$code_neu, levels = c("anderes <r>", "r3"))


#r
(t_r <- table(d_r$code_neu, d_r$gsyll_struc))

#fisher
(test <- fisher.test(t_r)) #no difference
assocstats(t_r)
chisq.posthoc.test(t_r)

png("assocplot_r3_distinktAufPos.png")
assocplot(t_r)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("r3-Form_Silbenposition", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")
