#graph_contingency.R
#analysis of graphical letter forms and their correlations with grammatical structures
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(plyr)){install.packages("plyr")}
library(tidyverse)

source("scripts/dataHandling.R")
source("scripts/contingencyTests.R")

options(scipen = 999)
#create a notin-operator
`%notin%` <- Negate(`%in%`)

#Distinctivity at syllable positions ====

#load data
d_dist <- data.loadData(whichColumns = c("letter_rec", "code", "gsyll_struc"), removeWaZ = F, removeUpperCase = T, removeUnrecognisable = T)

#reorder the factor levels of gsyll_struc
d_dist$gsyll_struc <- factor(d_dist$gsyll_struc, levels = c("ONS", "NUC", "KEY", "CODA", "EXTRA"))

#  test single letters ====
test_letters <- c("e")

for (letter in test_letters)
{
  d_dist_temp <- filter(d_dist, letter_rec == letter)
  d_dist_temp$letter_rec = NULL
  d_dist_temp <- droplevels(d_dist_temp)
  #get table and contingency tests
  (table(d_dist_temp))
  cont_test(d_dist_temp, x.title = paste0("gsyll_",letter), y.title = "form")
}
#clean up
rm(letter, test_letters, d_dist_temp)


#  test multiple letters and compare them ====
letter_pairs <- list(c("a5", "o3"), 
                     c("f7", "t3"), 
                     c("f9", "h6", "l2", "t4"),
                     c("h1", "k3"),
                     c("h5", "l1"),
                     c("n1", "u1"),
                     c("r2", "v1"))

for (pair in letter_pairs)
{
  print(pair)
  #get letter recs
  letter_recs <- substr(pair, 1,1)
  
  d_dist_temp <- droplevels(filter(d_dist, letter_rec %in% letter_recs))
  
  #we need to cast "code" to characters for now, so we can add a new factor level more easily
  d_dist_temp$code <- as.character(d_dist_temp$code)
  
  #contrast the reduced form to every other form
  d_dist_temp$code[d_dist_temp$code %notin% pair] <- "other form"
  
  #recast to factor
  d_dist_temp$code <- as.factor(d_dist_temp$code)
  
  #make sure "other form" is at the start of the level list
  forcats::fct_relevel(d_dist_temp$code, "other form", after = 0)

  #now send the individual letters for checking
  for (form in pair)
  {
    print(form)
    letter <- substr(form, 1, 1)
    
    d_dist_temp2 <- filter(d_dist_temp, letter_rec == letter)
    d_dist_temp2$letter_rec = NULL
    d_dist_temp2 <- droplevels(d_dist_temp2)
    
    #get table and contingency tests
    (table(d_dist_temp2))
    cont_test(d_dist_temp2, x.title = paste0("gsyll_",form), y.title = "form")
  }
}
#clean up
rm(d_dist, d_dist_temp, d_dist_temp2, letter_pairs, form, letter, letter_recs, pair)


#Complex graphemes ====

#load data
d_complex <- data.loadData(whichColumns = c("junc_border", "graph_complexity", "letter_rec", "code", "graph_complexity_both"))

#rename some factor levels
d_complex$junc_border <- plyr::revalue(d_complex$junc_border, c("TRUE" = "separation", "FALSE" = "connection"))

#save dataset with pseudo-complex graphemes and with letters for later use
d_complex_pseudo <- d_complex
d_complex_withLetter <- d_complex

#rename some factor levels
d_complex$graph_complexity <- plyr::revalue(d_complex$graph_complexity, c("pseudo-rh" = "FALSE", "pseudo-th" = "FALSE", "pseudo-st" = "FALSE"))

#sch is a trinary complex grapheme, so it will be discarded for now
d_complex <- filter(d_complex, !graph_complexity == "sch")
d_complex <- droplevels(d_complex)

#remove letter_rec, code, and graph_complexity_both where they're not needed
d_complex$letter_rec <- NULL
d_complex$code <- NULL
d_complex$graph_complexity_both <- NULL
d_complex_pseudo$letter_rec <- NULL
d_complex_pseudo$code <- NULL
d_complex_pseudo$graph_complexity_both <- NULL
d_complex <- droplevels(d_complex)
d_complex_pseudo <- droplevels(d_complex_pseudo)

#get a frequency table
table(d_complex$graph_complexity)

#  Contrast complex graphemes against single letter graphemes ====
d_complex_binary <- d_complex
newValues <- c("ch" = "complex",
               "rh" = "complex",
               "ck" = "complex", 
               "pf" = "complex",
               "ph" = "complex",
               "qu" = "complex",
               "st" = "complex",
               "th" = "complex",
               "el" = "not complex",
               "ng" = "not complex",
               "FALSE" = "not complex")
d_complex_binary$graph_complexity <- plyr::revalue(d_complex_binary$graph_complexity, newValues)
d_complex_binary <- droplevels(d_complex_binary)

#get frequency table and run contingency test
table(d_complex_binary)
cont_test(data = d_complex_binary, x.title = "complex", y.title = "junction")

#clean up
rm(d_complex_binary)

#  Contrast <th> and pseudo-<th> ====
d_complex_th <- filter(d_complex_pseudo, graph_complexity %in% c("th", "pseudo-th"))
d_complex_th <- droplevels(d_complex_th)

#get frequency table and run contingency test
table(d_complex_th)
cont_test(d_complex_th, x.title = "complex_th", y.title = "junction")

#clean up
rm(d_complex_th)
rm(d_complex_pseudo)

#  Compare complex graphemes individually ====
#<rh> is too rare, so it's omitted
d_complex_single <- filter(d_complex, !graph_complexity == "rh")

#we will also remove <el> and <ng> for now
d_complex_single <- filter(d_complex_single, !graph_complexity == "ng")
d_complex_single <- filter(d_complex_single, !graph_complexity == "el")

#remove all non-complex graphemes
d_complex_single <- filter(d_complex_single, !graph_complexity == "FALSE")
d_complex_single <- droplevels(d_complex_single)

#get frequency table and run contingency test
table(d_complex_single)
cont_test(data = d_complex_single, x.title = "complex_onlyComplex", y.title = "junction")

#clean up
rm(d_complex_single)

#  Analyse <sch> ====
#select <sch> cases
d_complex_sch <- filter(d_complex_withLetter, graph_complexity %in% c("sch", "FALSE"))
d_complex_sch$letter_rec <- NULL
d_complex_sch$code <- NULL
d_complex_sch$graph_complexity_both <- NULL
d_complex_sch <- droplevels(d_complex_sch)

#get frequency table and run contingency test
table(d_complex_sch)
cont_test(d_complex_sch, x.title = "complex_sch", y.title = "junction")

#clean up
rm(d_complex_sch)

#separation between s and ch
d_complex_sc <- filter(d_complex_withLetter, letter_rec %in% c("s", "c"))
d_complex_sc <- filter(d_complex_sc, graph_complexity == "sch")
d_complex_sc$graph_complexity <- NULL
d_complex_sc$code <- NULL
d_complex_sc$graph_complexity_both <- NULL
d_complex_sc <- droplevels(d_complex_sc)

#get frequency table and run contingency test
table(d_complex_sc)
cont_test(d_complex_sc, x.title = "complex_sc", y.title = "junction")

#clean up
rm(d_complex_sc)

#  Case study: compare <ng> and <el> as potentially complex graphemes ====
#select <ng> cases
d_complex_ng <- filter(d_complex, graph_complexity %in% c("ng", "FALSE"))
d_complex_ng$graph_complexity <- ifelse(d_complex_ng$graph_complexity == "FALSE", "not <ng>", "<ng>")
d_complex_ng$graph_complexity <- factor(d_complex_ng$graph_complexity)

#get frequency table and run contingency test
table(d_complex_ng)
cont_test(d_complex_ng, x.title = "complex_ng", y.title = "junction")

#clean up
rm(d_complex_ng)

#select <el> cases
d_complex_el <- filter(d_complex, graph_complexity %in% c("el", "FALSE"))
d_complex_el$graph_complexity <- ifelse(d_complex_el$graph_complexity == "FALSE", "not <el>", "<el>")
d_complex_el$graph_complexity <- factor(d_complex_el$graph_complexity)


#get frequency table and run contingency test
table(d_complex_el)
cont_test(d_complex_el, x.title = "complex_el", y.title = "junction")

#clean up
rm(d_complex_el)

#  Are there special letter forms in complex graphemes? ====
#remove all letters not occuring in complex graphemes or not regognizable
test_letters <- c("k", "h", "t", "n", "g", "e", "l")
d_complex_letterForms <- filter(d_complex_withLetter, letter_rec %in% test_letters)
d_complex_letterForms <- filter(d_complex_letterForms, !str_detect(code, "99"))

d_complex_letterForms$graph_complexity <- NULL
d_complex_letterForms$junc_border <- NULL

#contrast complex graphemes and single letter graphemes
d_complex_letterForms$graph_complexity_both <- plyr::revalue(d_complex_letterForms$graph_complexity_both, c("ch" = "complex",
                                                                                                            "ck" = "complex",
                                                                                                            "FALSE" = "not complex",
                                                                                                            "th" = "complex",
                                                                                                            "ng" = "complex",
                                                                                                            "el" = "complex"))
d_complex_letterForms <- droplevels(d_complex_letterForms)

#since the frequencies are dependent on the overall letter frequency, I need to test for each letter individually
for(letter in test_letters)
{
  d_complex_letter <- filter(d_complex_letterForms, letter_rec == letter)
  d_complex_letter$letter_rec <- NULL
  d_complex_letter <- droplevels(d_complex_letter)
  
  print(table(d_complex_letter))
  cont_test(d_complex_letter, x.title = paste0(letter,"-form"), y.title = "complex")
}

#and that's the analysis of complexe graphemes
#clean up
rm(d_complex)
rm(d_complex_letter)
rm(d_complex_letterForms)
rm(d_complex_withLetter)
rm(letter)
rm(newValues)
rm(test_letters)
