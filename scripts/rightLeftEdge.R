#eFormFunction.R
#check if there are striking letter shapes at the end of a word
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function for analyzing reduced letters
checkRedLetter <- function(letter = "a", red_forms, data = d, fisher = F, fontsize = 15) 
{
  #only keep necessary columns
  d_subs <- droplevels(filter(data, letter_rec == letter))
  
  #recode letter forms to "reduced" and "not reduced"
  d_subs$code_neu <- ifelse(d_subs$code_neu %in% red_forms, "reduziert", "nicht reduziert")
  
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$word_struc))
  
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
  
  #save graphics
  filename <- paste0("assocplot_",letter,"_reduziert_Endrand.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"ReduziertEndrand"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}


#function for analyzing each letter
checkLetter <- function(letter = "a", data = d, fisher = F, fontsize = 15) 
{
  #only keep necessary columns
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$word_struc))
  
  #calculate contingency test
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
  
  #save graphic output
  filename <- paste0("assocplot_",letter,"_Endrand.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"Endrand"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#keep only necessary columns
d <- dplyr::select(d, letter_rec, code_neu, word_struc, junc_border, junc_border_before, WaZ)

str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$word_struc <- as.factor(d$word_struc)
d$junc_border_before <- as.factor(d$junc_border_before)
d$junc_border <- as.factor(d$junc_border)

#backup for later use with the junctions
d_junc <- d

#remove upper-case and unrecognizable letters
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))

#rephrase some variable levels
d$word_struc <- plyr::revalue(d$word_struc, c("ini" = "initial", "body" = "medial", "fin" = "final"))
d$word_struc <- factor(d$word_struc, c("initial", "medial", "final"))
d <- droplevels(d)

summary(d)

#get table
table(d$code_neu, d$word_struc)

####letter shape of last letter####

#analyze letters
checkLetter("a", fisher = T) #significant
checkLetter("b", fisher = T) #probably won't survive holm correction
checkLetter("d") #significant
checkLetter("f") #significant
checkLetter("g") #significant
checkLetter("h") #significant
checkLetter("j") #not significant
checkLetter("k", fisher = T) #probably won't survive holm correction
checkLetter("l") #significant
checkLetter("m") #probably won't survive holm correction
checkLetter("n") #significant
checkLetter("o", fisher = T) #not significant
checkLetter("p", fisher = T) #probably won't survive holm correction
checkLetter("r", fisher = T) #significant
checkLetter("s") #significant
checkLetter("t", fisher = T) #significant
checkLetter("u") #significant
checkLetter("w") #probably won't survive holm correction
checkLetter("x", fisher = T) #not significant
checkLetter("y", fisher = T) #not significant
checkLetter("z", fisher = T) #might not survive holm correction
checkLetter("ß", fisher = T) #nope



#analyze reduced letters
checkRedLetter("a", red_forms = "a5", fisher = F) #not significant
checkRedLetter("f", red_forms = c("f5","f6", "f7", "f9"), fisher = F) #might not survive holm correction
checkRedLetter("h", red_forms = c("h5","h6"), fisher = F) #significant
checkRedLetter("k", red_forms = c("k3"), fisher = F) #not significant
checkRedLetter("r", red_forms = c("r2", "r4"), fisher = F) #not significant
checkRedLetter("s", red_forms = c("s2"), fisher = F) #not significant
checkRedLetter("t", red_forms = c("t3"), fisher = F) #not significant


####junction before last letter and the first letter####
#is the second to last letter more frequently joined with the last one?

#use previosly backupped data set
d_pre <- d_junc

#remove line-break separations and the first letters
d_pre <- filter(d_pre, WaZ != "TRUE")
d_pre <- filter(d_pre, word_struc != "ini")
d_pre <- droplevels(d_pre)

summary(d_pre)

#chisq.test
t <- table(d_pre$word_struc, d_pre$junc_border_before)
chisq.test(t)
assocstats(t)
chisq.posthoc.test(t) #the last two letters are more often joined!!


#is the first letter more often separated than the letters within a word?

#remove line-break separations and the last letters
d_junc <- filter(d_junc, WaZ != "TRUE")
d_junc <- filter(d_junc, word_struc != "fin")
d_junc <- droplevels(d_junc)

summary(d_junc)
#chisq.test
t <- table(d_junc$word_struc, d_junc$junc_border)
chisq.test(t) #no, they are not more often separated


