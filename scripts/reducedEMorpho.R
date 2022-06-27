#reducedEMorpho.R
#determine if the reduced e occurs more in certain morpheme types
#© Niklas Reinken, July 2021
options(scipen = 999)

library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function to check e form correlation to morpheme type
checkLetter <- function(letter = "e", data = d, fisher = F, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$morph_cat))
  
  #calculate contingency tests
  if(fisher == T)
  {
    print("Fisher-Test")
    print(test <- fisher.test(t, simulate.p.value = T))
  }
  else
  {
    print("Chi-Quadrat-Test")
    print(test <- chisq.test(t))
  }
  
  #return if not significant
  if(test$p.value > 0.05)
  {
    return()
  }
  print("Effektstärke")
  print(assocstats(t))
  
  print("Post-Hocs")
  print(chisq.posthoc.test(t))
  
  #save graphic output
  filename <- paste0("assocplot_reduEForm_MorphCat.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"eFormMorphCat"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}


#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#use only necessary columns
d <- dplyr::select(d, gsyll_type, code_neu, letter_rec, morph_cat)

#factorize
d$gsyll_type <- as.factor(d$gsyll_type)
d$code_neu <- as.factor(d$code_neu)
d$letter_rec <- as.factor(d$letter_rec)
d$morph_cat <- as.factor(d$morph_cat)
str(d)

#remove upper-case and unrecognizable letters
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))

#only e in reduced syllables
d <- filter(d, letter_rec == "e")
d <- filter(d, gsyll_type == "RED")


#only use e in inflection affixes or in proper lexemes
d <- filter(d, morph_cat %in% c("FLEX", "LEX"))
d <- droplevels(d)

#rename factor levels
d$morph_cat <- plyr::revalue(d$morph_cat, c("LEX"= "morph. Rest/Pseudoaffix", "FLEX" = "Flexionsaffix" ))
d <- droplevels(d)
summary(d)

#compare e form depending on morpheme category
checkLetter("e")
