#junctionsPersons.R
#tests the junction frequency for each individual text
#based on scripts by Niklas Reinken, July 2021
#version 2, January 2023

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(plyr)){install.packages("plyr")}
library(tidyverse)

source("scripts/dataHandling.R")

options(scipen = 0)

#load data
d <- data.loadData(whichColumns = c("person_ID", "junc_border"), 
                   removeWaZ = T, removeWordEnds = T, removeUpperCase = F, removeUnrecognisable = F)

d$person_ID <- as.factor(d$person_ID)

prop.table(table(d$junc_border))

#group data by persons
d_groups <-  group_by(d, person_ID) %>%
  group_split()

#calculate junction rates for each person and save them in data frame
junc_rates <- data.frame()
for(person in d_groups)
{
  person <- droplevels(person)
  junc_rates <- rbind(junc_rates, c(levels(person$person_ID), 
                                    round(prop.table(table(person$junc_border)),3)))
}
colnames(junc_rates) <- c("ID", "connected", "not connected")
junc_rates <- junc_rates[order(junc_rates$connected),]

#write data frame to .csv
write_csv2(junc_rates, "results/junctionRates_person.csv")

#group handwritings to cursive, block, or mixed
breaks <- c(0,0.2,0.8,1)
tags <- c("block","mixed", "cursive")
group_tags <- cut(as.numeric(junc_rates$connected), 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
junc_rates$group <- group_tags
# inspect bins
summary(group_tags)

#plot density
ggplot(data = junc_rates, aes(x = as.numeric(connected))) +
  geom_histogram(binwidth = 0.05, alpha = 0.5, fill = "#222222") +
  geom_density(color = "black") +
  labs(x = "junction rate", y = "number of texts") +
  theme_minimal()
ggsave("graphs/density_junctionRates.eps", width = 5, height = 3, device=cairo_ps)
ggsave("graphs/density_junctionRates.png", width = 5, height = 3)
