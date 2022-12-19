#graph_contingency.R
#analysis of graphical letter forms and their correlations with grammatical structures
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(plyr)){install.packages("plyr")}
library(tidyverse)

source("scripts/dataHandling.R")
source("scripts/contingencyTests.R")

options(scipen = 999)

# Complex graphemes ====

#load data
data_complexGraphemes <- data.loadData(whichColumns = c("junc_border", "graph_complexity", "letter_rec"))

#rename some factor levels
data_complexGraphemes$junc_border <- plyr::revalue(data_complexGraphemes$junc_border, c("TRUE" = "separation", "FALSE" = "connection"))

#save dataset with pseudo-complex graphemes and with <sch> for later use
data_complexGraphemes_pseudo <- data_complexGraphemes
data_complexGraphemes_withLetter <- data_complexGraphemes

#rename some factor levels
data_complexGraphemes$graph_complexity <- plyr::revalue(data_complexGraphemes$graph_complexity, c("pseudo-rh" = "FALSE", "pseudo-th" = "FALSE", "pseudo-st" = "FALSE"))

#sch is a trinary complex grapheme, so it will be discarded for now
data_complexGraphemes <- filter(data_complexGraphemes, !graph_complexity == "sch")
data_complexGraphemes <- droplevels(data_complexGraphemes)

#remove letter_rec where it's not needed
data_complexGraphemes$letter_rec <- NULL
data_complexGraphemes_pseudo$letter_rec <- NULL
data_complexGraphemes <- droplevels(data_complexGraphemes)
data_complexGraphemes_pseudo <- droplevels(data_complexGraphemes_pseudo)

#get a frequency table
table(data_complexGraphemes$graph_complexity)

#  contrast complex graphemes against single letter graphemes ====
data_complexGraphemes_binary <- data_complexGraphemes
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
data_complexGraphemes_binary$graph_complexity <- plyr::revalue(data_complexGraphemes_binary$graph_complexity, newValues)
data_complexGraphemes_binary <- droplevels(data_complexGraphemes_binary)

#get frequency table and run contingency test
table(data_complexGraphemes_binary)
cont_test(data = data_complexGraphemes_binary, x.title = "complexity", y.title = "junction")

#  contrast th and pseudo-th ====
data_complexGraphemes_th <- filter(data_complexGraphemes_pseudo, graph_complexity %in% c("th", "pseudo-th"))
data_complexGraphemes_th <- droplevels(data_complexGraphemes_th)

#get frequency table and run contingency test
table(data_complexGraphemes_th)
cont_test(data_complexGraphemes_th, x.title = "complexity_th", y.title = "junction")


#  compare complex graphemes individually ====
#<rh> is too rare, so it's omitted
data_complexGraphemes_single <- filter(data_complexGraphemes, !graph_complexity == "rh")

#we will also remove <el> and <ng> for now
data_complexGraphemes_single <- filter(data_complexGraphemes_single, !graph_complexity == "ng")
data_complexGraphemes_single <- filter(data_complexGraphemes_single, !graph_complexity == "el")

#remove all non-complex graphemes
data_complexGraphemes_single <- filter(data_complexGraphemes_single, !graph_complexity == "FALSE")
data_complexGraphemes_single <- droplevels(data_complexGraphemes_single)

#get frequency table and run contingency test
table(data_complexGraphemes_single)
cont_test(data = data_complexGraphemes_single, x.title = "complexity_onlyComplex", y.title = "junction")

#  analyse <sch> ====
#select <sch> cases
data_complexGraphemes_sch <- filter(data_complexGraphemes_withLetter, graph_complexity %in% c("sch", "FALSE"))
data_complexGraphemes_sch$letter_rec <- NULL
data_complexGraphemes_sch <- droplevels(data_complexGraphemes_sch)

#get frequency table and run contingency test
table(data_complexGraphemes_sch)
cont_test(data_complexGraphemes_sch, x.title = "complexity_sch", y.title = "junction")

#separation between s and ch
data_complexGraphemes_sc <- filter(data_complexGraphemes_withLetter, graph_complexity == "sch")
data_complexGraphemes_sc <- filter(data_complexGraphemes_sc, letter_rec %in% c("s", "c"))
data_complexGraphemes_sc$graph_complexity <- NULL
data_complexGraphemes_sc <- droplevels(data_complexGraphemes_sc)

#get frequency table and run contingency test
table(data_complexGraphemes_sc)
cont_test(data_complexGraphemes_sc, x.title = "complexity_sc", y.title = "junction")

#  case study: compare <ng> and <el> as potentially complex graphemes ====
#select <ng> cases
data_complexGraphemes_ng <- filter(data_complexGraphemes, graph_complexity %in% c("ng", "FALSE"))
data_complexGraphemes_ng$graph_complexity <- ifelse(data_complexGraphemes_ng$graph_complexity == "FALSE", "not <ng>", "<ng>")

#get frequency table and run contingency test
table(data_complexGraphemes_ng)
cont_test(data_complexGraphemes_ng, x.title = "complexity_ng", y.title = "junction")

#select <el> cases
data_complexGraphemes_el <- filter(data_complexGraphemes, graph_complexity %in% c("el", "FALSE"))
data_complexGraphemes_el$graph_complexity <- ifelse(data_complexGraphemes_el$graph_complexity == "FALSE", "not <el>", "<el>")

#get frequency table and run contingency test
table(data_complexGraphemes_el)
cont_test(data_complexGraphemes_el, x.title = "complexity_el", y.title = "junction")
