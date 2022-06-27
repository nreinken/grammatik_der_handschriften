#morphemeBoundaries.R
#checks if letters at morpheme boundaries are typically joined or not
#© Niklas Reinken, June 2021
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
d <- dplyr::select(d, junc_border, morph_border, morph_process_type, WaZ, word_struc)

#factorize
d$junc_border <- as.factor(d$junc_border)
d$morph_border <- as.factor(d$morph_border)
d$morph_process_type <- as.factor(d$morph_process_type)
d$word_struc <- as.factor(d$word_struc)
d$WaZ <- as.logical(d$WaZ)
str(d)

#remove word ends and line break separations
d <- filter(d, !WaZ)
d <- filter(d, word_struc != "fin")
d$WaZ <- NULL
d$word_struc <- NULL
d <- droplevels(d) 

summary(d)


#test if morpheme boundaries are more often separated
d_morph <- d

#contingency
(t_morph <- table(d_morph$junc_border, d_morph$morph_border))
(test <- chisq.test(t_morph))  ##significant, but probably won't survive holm correction
assocstats(t_morph)
chisq.posthoc.test(t_morph)

#plot graphics
png("assocplot_juncBorder_morphBorder.png")
assocplot(t_morph)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungMorphologischeGrenze", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


#check if the morphological processes differ in terms of letter junctions

#remove UNI because it's too rare
d_morph <- droplevels(filter(d, !morph_process_type == "UNI"))

#rename variable levels
d_morph$junc_border <- plyr::revalue(d_morph$junc_border, c("TRUE" = "unterbrochen", "FALSE" = "verbunden"))
d_morph$morph_process_type <- plyr::revalue(d_morph$morph_process_type, c("n.V." = "keine Grenze", "flexi" = "Flexion", "deri" = "Derivation", "compo" = "Komposition"))

#contingency test
(t_morph <- table(d_morph$junc_border, d_morph$morph_process_type))
(test <- chisq.test(t_morph))  ##significant, but probably won't survive holm correction
assocstats(t_morph)
chisq.posthoc.test(t_morph)

#save plot
png("assocplot_juncBorder_morphProcess.png", pointsize = 12)
assocplot(t_morph)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungMorphProcess", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")
