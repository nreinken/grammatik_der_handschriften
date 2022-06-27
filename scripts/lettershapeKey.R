#hShapeFunction.R
#checks if h shape and the different functions of h are correlated
#© Niklas Reinken, June 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function for contingency tests for each letter by syllable structur
checkLetter <- function(letter = "a", data = d, fisher = F) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$gsyll_struc))
  
  #contingency tests
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
  print("Effektstärke")
  print(assocstats(t))
  
  print("Post-Hocs")
  print(chisq.posthoc.test(t))
  
  #save plot
  filename <- paste0("assocplot_",letter,"_key.png")
  png(filename)
  assocplot(t)
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"Key"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}

#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#remove unnecessary columns
d <- dplyr::select(d, letter_rec, code_neu, gsyll_struc)
str(d)

#factorize
d$letter_rec <- as.factor(d$letter_rec)
d$code_neu <- as.factor(d$code_neu)
d$gsyll_struc <- as.factor(d$gsyll_struc)

#omit upper-case and unrecognizable letters
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))


#contrast key-position against all other positions
d$gsyll_struc <- plyr::revalue(d$gsyll_struc, c("ONS" = "nicht KEY", 
                                                "NUC" = "nicht KEY", 
                                                "EXTRA" = "nicht KEY", 
                                                "CODA" = "nicht KEY"))
d <- droplevels(d) 
summary(d)


#analyze letters
checkLetter("a", fisher = T) #a not significant
checkLetter("b", fisher = F) #b not significant
#c does only occur in one shape
checkLetter("d", fisher = F) #d significant, but probably won't survive holm correction
checkLetter("e", fisher = F) #e significant!
checkLetter("f", fisher = F) #f significant!
checkLetter("g", fisher = F) #g not significant
checkLetter("h", fisher = F) #h not significant
#i does only occur in one shape
#j kommt nicht in KEY vor
checkLetter("k", fisher = F) #k significant, but probably won't survive holm correction
checkLetter("l", fisher = F) #l not significant
checkLetter("m", fisher = F) #m not significant
checkLetter("n", fisher = F) #n not significant
checkLetter("o", fisher = T) #o not significant
checkLetter("p", fisher = F) #p not significant
#q does only occur in one shape
checkLetter("r", fisher = F) #r significant, but probably won't survive holm correction
checkLetter("s", fisher = T) #s significant!
checkLetter("t", fisher = T) #t significant!
checkLetter("u", fisher = F) #u significant, but probably won't survive holm correction
#v does only occur in one shape
checkLetter("w", fisher = T) #w not significant
checkLetter("x", fisher = T) #x not significant
checkLetter("y", fisher = T) #y not significant
checkLetter("z", fisher = T) #z not significant
checkLetter("ß", fisher = T) #ß not significant

