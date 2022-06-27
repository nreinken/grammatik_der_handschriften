#doubleconsonants.R
#determine if the parts of a double consonants have the same basic shape
#© Niklas Reinken, July 2021
options(scipen = 999)

library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function for analyzing each letter (parameters: the letter, the dataset, if the fisher-test is to be used, the fontsize for output graphic files)
checkLetter <- function(letter = "a", data = d, fisher = T, fontsize = 15) 
{
  #print a table of frequencies
  d_subs <- droplevels(filter(data, doppelt == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$doppelt_index))
  
  #calculate a contingency test
  if(fisher == T)
  {
    print("Fisher-Test")
    print(test <- fisher.test(t))
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
  #calculate effect sizes
  print("Effektstärke")
  print(assocstats(t))
  
  #run post-hoc tests
  print("Post-Hocs")
  print(chisq.posthoc.test(t))
  
  #write graphics
  filename <- paste0("assocplot_",letter,"_doppelkonsonant.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"Doppelkonsonant"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}

#check if the same shape is used in a double consonant (parameters: the double consonants to be checked)
get_same_forms <- function(doppelter = c("ff", "ll", "mm", "nn", "rr", "ss", "tt"))
{
  #select first double consonant in parameter list
  same_form <- NULL
  ff <- droplevels(subset(d, d$doppelt %in% doppelter))
  for(index in 1:nrow(ff))
  {
    case <- ff[index,]
    if(case$doppelt_index == 1)
    {
      #go to the next case (that is the second part of the double consonant) and check wether it has the same shape
      case_next <- ff[index + 1,]
      same_form <- rbind(same_form, case$code_neu == case_next$code_neu) 
    }
  }
  #output
  print(doppelter)
  print(table(same_form))
  print(round(prop.table(table(same_form)),3))
}

#check, if there is a difference between real double consonants and pseudo double consonants (i.e. morphem borders)
check_same_forms_pseudo <- function(doppelter = "ff", pseudo = "pseudo-ff")
{
  same_form <- NULL
  cases <- c(doppelter, pseudo)
  ff <- droplevels(subset(d, d$doppelt %in% cases))
  #select row
  for(index in 1:nrow(ff))
  {
    case <- ff[index,]
    if(case$doppelt_index == 1)
    {
      #check if the next case has the same shape
      case_next <- ff[index + 1,]
      same_form <- data.frame(rbind(same_form, c(as.character(case$doppelt), case$code_neu == case_next$code_neu))) 
    }
    colnames(same_form) <- c("Fall", "Gleich")
  }
  #print results
  print(c(doppelter, " und ", pseudo))
  print("Häufigkeiten:")
  
  #apply fisher to test for group differences
  print(t <- table(same_form$Fall, same_form$Gleich))
  
  print("Fisher-Test")
  print(test <- fisher.test(t))
  
  #return if not significant
  if(test$p.value > 0.05)
  {
    return()
  }
  
  #effect sizes
  print("Effektstärke")
  print(assocstats(t))
  
  #post hocs
  print("Post-Hocs")
  print(chisq.posthoc.test(t))
  
  #create plot file
  filename <- paste0("assocplot_",doppelter,"_gleicheFormen.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(doppelter,"Doppelt"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}

#START OF SCRIPT#

#data handling
  data <- read_csv2("../Graphen_MAIN.csv")
  d <- remove_empty(data, which = c("rows", "cols"))
  
  #select only necessary columns
  d <- dplyr::select(d, letter_rec, code_neu, doppelt, doppelt_index)
  
  str(d)
  
  #factorize
  d$letter_rec <- as.factor(d$letter_rec)
  d$code_neu <- as.factor(d$code_neu)
  d$doppelt <- as.factor(d$doppelt)
  d$doppelt_index <- as.factor(d$doppelt_index)
  
  #filter upper-case und unrecognizable letters
  d <- filter(d, letter_rec != "0")
  d <- filter(d, !str_detect(code_neu, "99"))
  
  
  d <- droplevels(d)
  
  summary(d)
  
  #get table
  table(d$doppelt)

#analyze each double consonant for differences between first and second part
checkLetter("ff")
checkLetter("ll")
checkLetter("mm")
checkLetter("nn")
checkLetter("rr")
checkLetter("ss") 
checkLetter("tt")
#ss and tt are the only double consonant that show some kind of differences, but it won't survive the holm-correction

#check if the double consonant parts have the same shape
for(level in levels(d$doppelt))
{
 get_same_forms(level) #they usually do have the same shape
}
#apply function for all the double consonant cases
get_same_forms() #they usually do have the same shape


#check if there are differences between real double consonants and pseudo double consonants
check_same_forms_pseudo("ff", "pseudo-ff")
check_same_forms_pseudo("ll", "pseudo-ll")
check_same_forms_pseudo("nn", "pseudo-nn")
check_same_forms_pseudo("rr", "pseudo-rr")
check_same_forms_pseudo("ss", "pseudo-ss")
check_same_forms_pseudo("tt", "pseudo-tt")
#no, there is no difference between pseudo-couble and real double consonants.
