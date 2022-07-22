#complexGraphemes.R
#checks if complex graphemes have a different letter shape
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(tidyverse)
library(janitor)
library(vcd)
library(chisq.posthoc.test)

#load data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d, which = c("rows", "cols"))

#select only necessary columns
d <- dplyr::select(d, letter_rec, junc_border, WaZ, word_struc, graph_complexity)
str(d)

#factorize
d$word_struc <- as.factor(d$word_struc)
d$letter_rec <- as.factor(d$letter_rec)
d$graph_complexity <- as.factor(d$graph_complexity)
d$junc_border <- as.factor(d$junc_border)

#remove line break separations
d <- filter(d, d$WaZ != T)
d$word_struc <- NULL
d$WaZ <- NULL

#rename some factor levels
d$junc_border <- plyr::revalue(d$junc_border, c("TRUE" = "separation", "FALSE" = "connection"))
d <- droplevels(d)
glimpse(d)
summary(d)

#save data for later use with letter shapes
d2 <- d

#rename some factor levels
d2$graph_complexity <- plyr::revalue(d2$graph_complexity, c("pseudo-rh" = "FALSE", "pseudo-th" = "FALSE", "pseudo-st" = "FALSE"))

#sch is a trinary complex grapheme, so it will be discarded for now
d2 <- filter(d2, !graph_complexity == "sch")
d2 <- droplevels(d2)
summary(d2)
table(d2$graph_complexity)

####contrast complex graphemes against single letter graphemes####
d_binary <- d2
newValues <- c("ch" = "complex",
               "rh" = "complex",
               "ck" = "complex", 
               "pf" = "complex",
               "ph" = "complex",
               "qu" = "complex",
               "st" = "complex",
               "th" = "complex",
               "el" = "not complex",
               "ng" = "not complex",
               "FALSE" = "not complex")
d_binary$graph_complexity <- plyr::revalue(d_binary$graph_complexity, newValues)
d_binary <- droplevels(d_binary)
summary(d_binary)

#contingency test
(t_binary <- table(d_binary$junc_border,d_binary$graph_complexity))
(test <- chisq.test(t_binary)) #significant
assocstats(t_binary)
chisq.posthoc.test(t_binary)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("full_komplecityXconnection", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#save assocplots with and without legend
png("assoc_full_complexXconnection_noSCH.png", height = 400, width = 300)
assoc(t_binary, shade = T, legend_args = list("text" = "Pearson's\nresiduals"))
dev.off()
png("assocplot_full_complexXConnection_noSCH.png", height = 400, width = 300)
assocplot(t_binary)
dev.off()

####compare complex graphemes individually####
#rh is too rare, so it's omitted
d_single <- filter(d2, !graph_complexity == "rh")
d_single <- filter(d_single, !graph_complexity == "FALSE")
d_single <- droplevels(d_single)
summary(d_single)
table(d_single$graph_complexity)

#contingency test 
(t_single <- table(d_single$junc_border, d_single$graph_complexity))
(test <- chisq.test(t_single)) #significant
assocstats(t_single) #big effect
chisq.posthoc.test(t_single)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("single_complexXconnection", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#save plot
png("assocplot_complexXconnection_noSCH.png", height = 400, width = 300)
assocplot(t_single)
dev.off()


####case study: compare <ng> and <el> as potentially complex graphemes####

#select ng cases
d_ng <- filter(d, d$graph_complexity %in% c("ng", "FALSE"))
d_ng$graph_complexity <- ifelse(d_ng$graph_complexity == "FALSE", "not <ng>", "<ng>")
(t_ng <- table(d_ng$graph_complexity, d_ng$junc_border))

#chisq.test
(test <- chisq.test(t_ng))
assocstats(t_ng)
chisq.posthoc.test(t_ng)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("ng_connections", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#select el cases
d_el <- filter(d, d$graph_complexity %in% c("el", "FALSE"))
d_el$graph_complexity <- ifelse(d_el$graph_complexity == "FALSE", "not <el>", "<el>")

#contingency test
(t_el <- table(d_el$graph_complexity, d_el$junc_border))
test <- (chisq.test(t_el))
assocstats(t_el)
chisq.posthoc.test(t_el)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("el_connections", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")



####contrast th and pseudo-th####
d_th <- filter(d, d$graph_complexity %in% c("th", "pseudo-th"))
d_th <- droplevels(d_th)
summary(d_th)

#contingency table
(t_th <- table(d_th$junc_border, d_th$graph_complexity))
(test <- chisq.test(t_th))#significant
assocstats(t_th) #medium effect
chisq.posthoc.test(t_th)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("th_pseudoTh", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#save plot
png("assocplot_th_complexXconnection.png", height = 400, width = 300)
assocplot(t_th)
dev.off()

####and at last, sch####

#select sch cases
d_sch <- filter(d, d$graph_complexity %in% c("sch", "FALSE"))
d_sch$code_neu <- NULL
d_sch$graph_complexity <- plyr::revalue(d_sch$graph_complexity, c("FALSE" = "not complex"))
d_sch <- droplevels(d_sch)
summary(d_sch)

#contingency
(t_sch <- table(d_sch$junc_border, d_sch$graph_complexity))
(test <- chisq.test(t_sch))#significant
assocstats(t_sch) #medium effect
chisq.posthoc.test(t_sch)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("sch_connection", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#separation between s and ch
d_sc <- filter(d_sch, d_sch$graph_complexity == "sch")
d_sc <-  filter(d_sc, d_sc$letter_rec %in% c("s", "c"))
d_sc <- droplevels(d_sc)

#contingency test
(t_sc <- table(d_sc$junc_border, d_sc$letter_rec))
(test <- chisq.test(t_sc))#significant
assocstats(t_sc) #medium effect
chisq.posthoc.test(t_sc)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("sch_connection_sc", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#####letter shapes in complex graphemes####
#tidy up the environment a bit 
rm(list = ls())

#reload data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d)

#select only necessary columns
d <- dplyr::select(d, code_neu, letter_rec, graph_complexity_2)
str(d)

#factorize
d$code_neu <- as.factor(d$code_neu)
d$letter_rec <- as.factor(d$letter_rec)
d$graph_complexity_2 <- as.factor(d$graph_complexity_2)
d <- droplevels(d)

####are there special letter forms in complex graphemes?####
#remove all letters not occuring in complex graphemes or not regognizable
d <- filter(d, d$letter_rec %in% c("k", "h", "t", "n", "g", "e", "l"))
d <- filter(d, !str_detect(code_neu, "99"))

#backup dataset for later use in ng-el-comparison
d_ngel <- d #store for later use

#contrast complex graphemes and single letter graphemes
d$graph_complexity_2 <- plyr::revalue(d$graph_complexity_2, c("ch" = "complex",
                                                          "ck" = "complex",
                                                          "FALSE" = "not complex",
                                                          "th" = "complex",
                                                          "ng" = "not complex",
                                                          "el" = "not complex"))
d <- droplevels(d)
summary(d)

#since the frequencies are dependent on the overall letter frequency, I need to test for each letter individually
#create a dataframe for each letter
d_h <- droplevels(filter(d, d$letter_rec =="h"))
d_k <- droplevels(filter(d, d$letter_rec =="k"))
d_t <- droplevels(filter(d, d$letter_rec =="t"))

#tables
(t_h <- table(d_h$graph_complexity_2, d_h$code_neu))
(t_k <- table(d_k$graph_complexity_2, d_k$code_neu))
(t_t <- table(d_t$graph_complexity_2, d_t$code_neu))

#contingency test for h
(test <- chisq.test(t_h))
assocstats(t_h)
chisq.posthoc.test(t_h)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_h", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#contingency test for k
(test <- chisq.test(t_k))
assocstats(t_k)
chisq.posthoc.test(t_k)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_k", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#contingency test for t
(test <- fisher.test(t_t))
assocstats(t_t)
chisq.posthoc.test(t_t)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_t", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


#####check letter forms for el and ng####

#contrast complex graphemes with single letter graphemes
d_ngel$graph_complexity_2 <- plyr::revalue(d_ngel$graph_complexity_2, c("ch" = "not complex",
                                                              "ck" = "not complex",
                                                              "FALSE" = "not complex",
                                                              "th" = "not complex",
                                                              "ng" = "complex",
                                                              "el" = "complex"))
d_ngel <- droplevels(d_ngel)
summary(d_ngel)

#create dataframes for each letter (see above)
d_n <- droplevels(filter(d_ngel, d_ngel$letter_rec =="n"))
d_g <- droplevels(filter(d_ngel, d_ngel$letter_rec =="g"))
d_e <- droplevels(filter(d_ngel, d_ngel$letter_rec =="e"))
d_l <- droplevels(filter(d_ngel, d_ngel$letter_rec =="l"))

#tables
(t_n <- table(d_n$graph_complexity_2, d_n$code_neu))
(t_g <- table(d_g$graph_complexity_2, d_g$code_neu))
(t_e <- table(d_e$graph_complexity_2, d_e$code_neu))
(t_l <- table(d_l$graph_complexity_2, d_l$code_neu))

#contingency test for n
(test <- chisq.test(t_n))
assocstats(t_n)
chisq.posthoc.test(t_n)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_n", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#contingency test for g
(test <- chisq.test(t_g))
assocstats(t_g)
chisq.posthoc.test(t_g)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_g", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#contingency test for e
(test <- chisq.test(t_e))
assocstats(t_e)
chisq.posthoc.test(t_e)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_e", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#contingency test for l
(test <- chisq.test(t_l))
assocstats(t_l)
chisq.posthoc.test(t_l)

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("complex_form_l", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

