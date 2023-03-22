#kappa.R
#calculate intra-rater reliability
#based on scripts by Niklas Reinken, August 2021
#version 1, January 2023
options(scipen = 999)

#load libraries
if (!requireNamespace("vcd", quietly = TRUE)) {
  install.packages("vcd")
}
library(vcd)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

#load functions
source("scripts/dataHandling.R")

#load data
d <- data.loadData(whichColumns = c("code", "code_val"), 
                   removeWaZ = F, removeWordEnds =F,
                   removeUpperCase = F, removeUnrecognisable = F)

#remove cases that have not been validated
d <- d %>% filter(!is.na(code_val))

#discard levels that do not occur in both ratings
common_levels <- intersect(levels(d$code), levels(d$code_val))
d$code <- factor(d$code, levels = common_levels)
d$code_val <- factor(d$code_val, levels = common_levels)

#show the levels
levels(d$code)
levels(d$code_val)
summary(d)

#calculate kappa
kappa <- vcd::Kappa(table(d))
cat("Kappa coefficient:\n")
print(kappa)


