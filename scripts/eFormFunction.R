#eFormFunction.R
#determine if there is a correlation between e basic shapes and e functions
#© Niklas Reinken, July 2021
options(scipen = 999)

library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function to calculate contingency tests for letter features (form and function) 
checkLetter <- function(letter = "e", data = d, fisher = F, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$e_func))
  
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
  
  #effect size
  print("Effektstärke")
  print(assocstats(t))
  
  #post hocs
  print("Post-Hocs")
  print(chisq.posthoc.test(t))
  
  #graphic output
  filename <- paste0("assocplot_eFunc.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"eFunc"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}


#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#select only necessary columns
d <- dplyr::select(d, code_neu, letter_rec, e_func)

#factorize
d$e_func <- as.factor(d$e_func)
d$code_neu <- as.factor(d$code_neu)
d$letter_rec <- as.factor(d$letter_rec)
str(d)

#remove upper-case and unrecognizable letters, keep only e
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))
d <- filter(d, letter_rec == "e")

#remove Umlaut-e and etymological e (because they are far to rare)
d <- filter(d, !e_func %in% c("UML", "ETYM"))

#sort factors
d$e_func <- factor(d$e_func, c("FULL", "DIPHTHONG", "LENGTH", "RED"))


d <- droplevels(d)
summary(d)

#check e form and e function
checkLetter("e", fontsize = 11)
