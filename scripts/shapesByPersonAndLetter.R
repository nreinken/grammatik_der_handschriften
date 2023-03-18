#shapesByPersonAndLetter.R
#calculates the amount of letter shapes each individual text contains for each letter
#based on scripts by Niklas Reinken, July 2021
#version 2, March 2023


#load libraries
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(plyr)){install.packages("plyr")}
library(tidyverse)

source("scripts/dataHandling.R")

options(scipen = 999)

#load data
d <- data.loadData(whichColumns = c("person_ID","letter_rec","code"), removeWaZ = F, removeWordEnds = F, removeUpperCase = T, removeUnrecognisable = T)


#group data for each letter shape
shapePersons <- group_by(d, code)
#count the amount of persons each letter is used by
shapePersons <- dplyr::summarize(shapePersons, nPersons = n_distinct(person_ID))
#save to .csv
write_csv2(shapePersons, "results/numberPersonsEachShape.csv")
#clean up 
rm(shapePersons)

#group by person and letter
d <- group_by(d, person_ID, letter_rec)
#count the amount of letter shapes for each person
d <- dplyr::summarize(d, nShapes = n_distinct(code))
#write to .csv
write_csv2(d, "results/numberShapesEachPerson.csv")

#group by person
persons <- group_by(d, person_ID)
#calculate median amount of letter shapes for each person
persons <- dplyr::summarize(persons, x = mean(nShapes))
median(persons$x)

#group by letter
letters <- group_by(d, letter_rec)
#calculate median amount of letter shapes for each letter
letters <- dplyr::summarize(letters, x = mean(nShapes))
median(letters$x)

#clean up
rm(d, letters, persons)
