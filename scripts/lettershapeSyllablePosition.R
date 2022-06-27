#lettershapeSyllablePosition.R
#checks if the letter shape is different at various syllable positions
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function for analyzing each letter
checkLetter <- function(letter = "a", data = d, fisher = F, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$gsyll_struc))
  
  #contingency test
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
  filename <- paste0("assocplot_",letter,"_silbenposition.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"Silbenposition"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}
#function for analyzing reduced letters
checkRedLetter <- function(letter = "a", red_forms, data = d_red, fisher = F, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  
  #select only reduced letters
  d_subs$code_neu <- ifelse(d_subs$code_neu %in% red_forms, "reduziert", "nicht reduziert")

  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$gsyll_struc))
  
  #contingency test
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
  filename <- paste0("assocplot_",letter,"reduziert_silbenposition.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"ReduziertSilbenposition"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#use only necessary columns
d <- dplyr::select(d, letter_rec, code_neu, gsyll_struc)
str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$gsyll_struc <- as.factor(d$gsyll_struc)

#remove upper-case and unrecognizable letters
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))
d <- droplevels(d)

#sort factor levels
d$gsyll_struc <- factor(d$gsyll_struc, c("ONS", "NUC", "KEY", "CODA", "EXTRA"))
summary(d)


#get table
table(d$code_neu, d$gsyll_struc)

#analyze letters
checkLetter("a", fisher = T) #a not significant
checkLetter("b", fisher = T) #b significant, but probably won't survive holm correction
#c hat nur eine Form
checkLetter("d", fisher = T, fontsize = 14) #d significant!
checkLetter("e", fisher = F) #e significant!
checkLetter("f", fisher = T, fontsize = 12) #f significant!
checkLetter("g", fisher = F, fontsize = 11) #g significant!
checkLetter("h", fisher = T) #h significant!
#i hat nur eine Form
checkLetter("j", fisher = T) #j not significant
checkLetter("k", fisher = F) #k significant, but probably won't survive holm correction
checkLetter("l", fisher = F) #l not significant
checkLetter("m", fisher = F) #m not significant
checkLetter("n", fisher = T) #n not significant
checkLetter("o", fisher = T) #o not significant
checkLetter("p", fisher = T) #p not significant
#q hat nur eine Form
checkLetter("r", fisher = F, fontsize = 12) #r significant!
checkLetter("s", fisher = F) #s significant!
checkLetter("t", fisher = F) #t significant!
checkLetter("u", fisher = T) #u significant, but probably won't survive holm correction
#v hat nur eine Form
checkLetter("w", fisher = T) #w not significant
checkLetter("x", fisher = T) #x not significant
checkLetter("y", fisher = T) #y not significant
checkLetter("z", fisher = T) #z ist auffällig
checkLetter("ß", fisher = T) #ß not significant


#analyze reduced forms
d_red <- d
checkRedLetter("a", red_forms = "a5", fisher = T)
checkRedLetter("e", red_forms = "e3", fisher = F)
checkRedLetter("f", red_forms = c("f5", "f6", "f7", "f9"), fisher = T)
checkRedLetter("g", red_forms = c("g5", "g6"), fisher = T)
checkRedLetter("h", red_forms = c("h5", "h6"), fisher = T)
checkRedLetter("k", red_forms = "k3", fisher = T)
checkRedLetter("o", red_forms = "o3", fisher = T)
checkRedLetter("r", red_forms = c("r2", "r4"), fisher = T)
checkRedLetter("s", red_forms = "s2", fisher = T)
checkRedLetter("t",red_forms = "t3", fisher = T)
