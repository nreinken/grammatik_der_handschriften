#kappa.R
#calculate intra rater reliability
#© Niklas Reinken, August 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(irr)

#load data
data <- read_csv2("Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#select only necessary columns and cases
d <- dplyr::select(d, code_neu, code_val)
d <- filter(d, !is.na(d$code_val))

#factorize
d$code_neu <- factor(d$code_neu)
d$code_val <- factor(d$code_val)
d <- droplevels(d)

#discard levels that do not occur in both ratings
d <- filter(d, d$code_neu %in% levels(d$code_val))
d <- droplevels(d)
d <- filter(d, d$code_val %in% levels(d$code_neu))
d <- filter(d, d$code_val != "k3")
d <- droplevels(d)
levels(d$code_neu)
levels(d$code_val)
str(d)
summary(d)

#calculate kappa
(t <- table(d))
(res.k <- Kappa(t))


