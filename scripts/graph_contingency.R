#graph_contingency.R
#analysis of graphical letter forms and their correlations with grammatical structures
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

source("scripts/dataHandling.R")

#load data
data <- data.loadData(whichColumns = c("letter_rec", "junc_border", "graph_complexity"))
