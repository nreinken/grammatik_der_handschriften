#shapesByPersonAndLetter.R
#calculates the amount of letter shapes each individual text contains for each letter
#© Niklas Reinken, July 2021

#load libraries
library(janitor)
library(tidyverse)


#load data
d <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(d)

#only use necessary columns
d <- select(d, person_ID, letter, code_neu)

#recode letter names for Umlaute
d$letter <- plyr::revalue(d$letter, c("ä" = "a", "ö" = "o", "ü" = "u"))

#group data for each letter shape
d_group_extract <- group_by(d, code_neu)
#count the amount of persons each letter is used by
personProForm <- summarize(d_group_extract, n = n_distinct(person_ID))
#save to .csv
write_csv2(personProForm, "PersonProForm.csv")

#omit unrecognizable letter shapes
d <- filter(d, !code_neu %in% c("0", "a99", "b99", "c99", "d99", "e99", "f99", "g99", "h99", "i99", "j99", "k99", "l99", "m99", "n99", "o99", "p99", "q99", "r99", "s99", "t99", "u99", "v99", "w99", "x99", "y99", "z99", "ß99"))

#group by person and letter
d_group <- group_by(d, person_ID, letter)
#count the amount of letter shapes for each person
formenProPerson <- summarize(d_group, n = n_distinct(code_neu))
#write to .csv
write_csv2(formenProPerson, "formenProPerson.csv")

#group by person
formenProPersonGroup <- group_by(formenProPerson, person_ID)
#calculate median amount of letter shapes for each person
formenProPersonGroupMedian <- summarize(formenProPersonGroup, x = mean(n))
median(formenProPersonGroupMedian$x)

#group by letter
formenProPersonLetter <- group_by(formenProPerson, letter)
#calculate median amount of letter shapes for each letter
formenProPersonLetterMedian <- summarize(formenProPersonLetter, x = mean(n))
