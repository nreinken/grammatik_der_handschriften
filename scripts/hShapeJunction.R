#hShapeJunction.R
#checks if h form is dependent on the existence of a letter junction
#© Niklas Reinken, June 2021
options(scipen = 999)

#load libraries
library(psych)
library(janitor)
library(tidyverse)

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data)

#select only necessary columns
d <- select(d, WaZ, letter, code_neu, junc_border_before, junc_border)

#select only letter h and omit line break separations
d <- filter(d, WaZ == F)
d <- filter(d, letter %in% c('h'))

#rename some variables
d <- mutate(d, lig = ifelse(code_neu %in% c("h3", "h4", "h6"),"Bogen", "kein Bogen"))
d <- mutate(d, junc_border_before = ifelse(junc_border_before, "unterbrochen", "verbunden"))
d <- mutate(d, junc_border = ifelse(junc_border, "unterbrochen", "verbunden"))

table(d$lig)
table(d$junc_border_before)

#chisq.test h with loop x separation before h
table(d$lig, d$junc_border_before)
(test <- chisq.test(d$lig, d$junc_border_before)) #significant
phi(table(d$lig, d$junc_border_before))

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("hForm_VerbindungDavor", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")

#chisq.test h with loop x separation after h
table(d$lig, d$junc_border)
(test <- chisq.test(d$lig, d$junc_border)) #significant
phi(table(d$lig, d$junc_border))

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("hForm_VerbindungDanach", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")
