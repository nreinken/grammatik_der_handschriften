#junctionsPersons.R
#tests the junction frequency for each individual text
#© Niklas Reinken, July 2021
options(scipen = 0)

#load libraries
library(janitor)
library(beepr)
library(tidyverse)

#load data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d)

#select only necessary columns
d <- dplyr::select(d, person_ID, junc_border, WaZ, word_struc)
str(d)

#factorize
d$junc_border <- as.factor(d$junc_border)
d$person_ID <- as.factor(d$person_ID)

#remove line break separations and last letters
d <- filter(d, d$WaZ != T)
d <- filter(d, word_struc != "fin")
d <- droplevels(d)
glimpse(d)
summary(d)


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
colnames(junc_rates) <- c("ID", "verbunden", "nicht verbunden")
junc_rates <- junc_rates[order(junc_rates$verbunden),]

#write data frame to .csv
write_csv2(junc_rates, "VerbindungsrateProPerson.csv")

#group handwritings to cursive, block, or mixed
breaks <- c(0,0.2,0.8,1)
tags <- c("unverbunden","teilverbunden", "verbunden")
group_tags <- cut(as.numeric(junc_rates$verbunden), 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
junc_rates$group <- group_tags
# inspect bins
summary(group_tags)

#plot density
ggplot(data = junc_rates, aes(x = as.numeric(verbunden))) +
  geom_histogram(binwidth = 0.05, alpha = 0.5, fill = "#222222") +
  geom_density(color = "darkred") +
  labs(x = "Verbundenheitsgrad", y = "Anzahl Texte") +
  theme_minimal()
ggsave("densityVerbundenheitsraten.png", width = 5, height = 3)
