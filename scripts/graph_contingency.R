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

####Complex graphemes####

#load data
data_complexGraphemes <- data.loadData(whichColumns = c("junc_border", "graph_complexity"))

#rename some factor levels
data_complexGraphemes$junc_border <- plyr::revalue(data_complexGraphemes$junc_border, c("TRUE" = "separation", "FALSE" = "connection"))
data_complexGraphemes$graph_complexity <- plyr::revalue(data_complexGraphemes$graph_complexity, c("pseudo-rh" = "FALSE", "pseudo-th" = "FALSE", "pseudo-st" = "FALSE"))

#sch is a trinary complex grapheme, so it will be discarded for now
data_complexGraphemes <- filter(data_complexGraphemes, !graph_complexity == "sch")
data_complexGraphemes <- droplevels(data_complexGraphemes)

#get a frequency table
table(data_complexGraphemes$graph_complexity)

#contrast complex graphemes against single letter graphemes
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

#get frequency table
table(data_complexGraphemes_binary)

#run contingency test
cont_test(data = data_complexGraphemes_binary, x.title = "Komplexität", y.title = "Verbindung")

#compare complex graphemes individually
#<rh> is too rare, so it's omitted
data_complexGraphemes_single <- filter(data_complexGraphemes, !graph_complexity == "rh")

#we will also remove <el> and <ng> for now
data_complexGraphemes_single <- filter(data_complexGraphemes_single, !graph_complexity == "ng")
data_complexGraphemes_single <- filter(data_complexGraphemes_single, !graph_complexity == "el")

#remove all non-complex graphemes
data_complexGraphemes_single <- filter(data_complexGraphemes_single, !graph_complexity == "FALSE")
data_complexGraphemes_single <- droplevels(data_complexGraphemes_single)

#get frequency table
table(data_complexGraphemes_single)
cont_test(data = data_complexGraphemes_single, x.title = "Komplexität_nurKomplexe", y.title = "Verbindung")
