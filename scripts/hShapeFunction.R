#hShapeFunction.R
#checks if h shape and the different functions of h are correlated
#© Niklas Reinken, June 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function to test the correlation between letter shape and 
checkLetter <- function(letter = "h", data = d, fisher = T, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$h_func))
  
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
  
  #save plot
  filename <- paste0("assocplot_hFunc.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"hFunc"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}


#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#select only necessary columns
d <- dplyr::select(d, code_neu, letter_rec, h_func)

#factorize
d$h_func <- as.factor(d$h_func)
d$code_neu <- as.factor(d$code_neu)
d$letter_rec <- as.factor(d$letter_rec)
str(d)

#omit upper case und unrecognizable letters, select only h
d <- filter(d, letter_rec != "0")
d <- filter(d, !str_detect(code_neu, "99"))

d <- filter(d, letter_rec == "h")

#sort factor levels
d$h_func <- factor(d$h_func, c("PHONO", "GRAPH", "SINI", "DEHN", "ETYM"))
d <- droplevels(d)
summary(d)

#analyze letter shape and function
checkLetter("h")

#plot
t <- data.frame(table(d$h_func, d$code_neu))
ggplot(data = t, aes(x = Var1, fill = Var2, y = Freq)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal()
  
