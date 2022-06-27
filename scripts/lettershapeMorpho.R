#lettershapeMorpho.R
#checks if the letter shape is different if the letter shows morphological processes
#© Niklas Reinken, July 2021
options(scipen = 999)

library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function for analyzing letters reduced letters
d_subs <- droplevels(filter(data, letter_rec == letter))
{  
  #contrast reduced and non-reduced letters
  d_subs$code_neu <- ifelse(d_subs$code_neu %in% red_forms, "reduziert", "nicht reduziert")
  
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$morph_belastet))
  
  #contingency tests
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
  
  #save graphics
  filename <- paste0("assocplot_",letter,"reduziert_morphBelastung.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"ReduziertMorphBelastung"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}
#function for analyzing each letter
checkLetter <- function(letter = "a", data = d, fisher = F, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$morph_belastet))
  
  #contingency
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
  
  #save graphics
  filename <- paste0("assocplot_",letter,"_morphBelastung.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"MorphBelastung"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#select only necessary columns
d <- dplyr::select(d, letter_rec, code_neu, morph_belastet)
str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$morph_belastet <- as.factor(d$morph_belastet)

#omit upper-case and unrecognizable letters
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))

#rename factor levels
d$morph_belastet <- plyr::revalue(d$morph_belastet, c("TRUE" = "belastet", "FALSE" = "nicht belastet"))
d <- droplevels(d)
summary(d)

#get table
table(d$code_neu, d$morph_belastet)

#analyze letters
checkRedLetter("a", red_forms = "a5", fisher = F) #not significant
checkRedLetter("f", red_forms = c("f5","f6", "f7", "f9"), fisher = F) #significant, but might not survive holm correction
checkRedLetter("h", red_forms = c("h5","h6"), fisher = F) #significant
checkRedLetter("k", red_forms = c("k3"), fisher = F) #not significant
checkRedLetter("r", red_forms = c("r2", "r4"), fisher = F) #not significant
checkRedLetter("s", red_forms = c("s2"), fisher = F) #not significant
checkRedLetter("t", red_forms = c("t3"), fisher = F) #not significant

#analyze letters
checkLetter("a")
checkLetter("b", fisher = T)
checkLetter("d")
checkLetter("f")
checkLetter("g")
checkLetter("h")
checkLetter("k", fisher = T)
checkLetter("l")
checkLetter("m")
checkLetter("n")
checkLetter("p", fisher = T)
checkLetter("r", fisher = T)
checkLetter("s")
checkLetter("t", fisher = T)
checkLetter("y")
checkLetter("ß", fisher = T)
