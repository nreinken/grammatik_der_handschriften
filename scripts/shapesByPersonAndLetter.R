# shapesByPersonAndLetter.R
# calculates the amount of letter shapes each individual text contains for each letter 
# based on scripts by Niklas Reinken, July 2021
# version 2, March 2023


# load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
}
library(tidyverse)


source("scripts/dataHandling.R")

options(scipen = 999)

# load data
d <- data.loadData(whichColumns = c("person_ID", "letter_rec", "code"), removeWaZ = F,
    removeWordEnds = F, removeUpperCase = T, removeUnrecognisable = T)


# calculate the number of persons each letter is used by
shapePersons <- d %>%
    group_by(code) %>%
    summarize(nPersons = n_distinct(person_ID)) %>%
    write_csv2("results/numberPersonsEachShape.csv")

# calculate the number of letter shapes for each person
numberShapesEachPerson <- d %>%
    group_by(person_ID, letter_rec) %>%
    summarize(nShapes = n_distinct(code)) %>%
    write_csv2("results/numberShapesEachPerson.csv")

# calculate the median amount of letter shapes for each person
medianShapesEachPerson <- numberShapesEachPerson %>%
    group_by(person_ID) %>%
    summarize(medianShapes = median(nShapes))

# calculate the median amount of letter shapes for each letter
medianShapesEachLetter <- numberShapesEachPerson %>%
    group_by(letter_rec) %>%
    summarize(medianShapes = median(nShapes))
