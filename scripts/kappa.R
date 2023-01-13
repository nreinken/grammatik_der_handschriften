#kappa.R
#calculate intra rater reliability
#based on scripts by Niklas Reinken, August 2021
#version 1, January 2023
options(scipen = 999)

#load libraries
if(!require(vcd)){install.packages("vcd")}

source("scripts/dataHandling.R")

#load data
d <- data.loadData(whichColumns = c("code", "code_val"), 
                   removeWaZ = F, removeWordEnds =F,
                   removeUpperCase = F, removeUnrecognisable = F)

#remove cases that have not been validated
d <- droplevels(filter(d, !is.na(d$code_val)))

#discard levels that do not occur in both ratings
d <- droplevels(filter(d, d$code %in% levels(d$code_val)))
d <- droplevels(filter(d, d$code_val %in% levels(d$code)))
d <- droplevels(filter(d, d$code_val != "k8")) #for some reasons, k8 is not discarded despite it being only in code_val

#show the levels
levels(d$code)
levels(d$code_val)
summary(d)

#calculate kappa
vcd::Kappa(t)


