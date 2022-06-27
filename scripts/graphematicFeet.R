#graphematicFeet.R
#checks wether graphematic feet are visible in handwriting by looking at letter shapes and junctions
#© Niklas Reinken, July 2021
options(scipen = 999)

#load libraries
library(janitor)
library(tidyverse)
library(vcd)
library(chisq.posthoc.test)

#function to check letter form and syllable type
checkLetter <- function(letter = "e", data = d2, fisher = F, fontsize = 15) 
{
  d_subs <- droplevels(filter(data, letter_rec == letter))
  print("Häufigkeiten:")
  print(t <- table(d_subs$code_neu, d_subs$gsyll_type))
  
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
  
  #save graphic output
  filename <- paste0("assocplot_",letter,"_reduktionssilbe.png")
  png(filename, pointsize = fontsize)
  assocplot(t(t))
  dev.off()
  
  #store p-value externally to apply bonferrroni-holm later
  p <- read.csv2("../pvalues.csv")
  p <- rbind(p, c(paste0(letter,"Reduktionssilbe"), test$p.value))
  colnames(p) <- c("Test", "p")
  write_csv2(p, "../pvalues.csv")
}


#load data
data <- read_csv2("../Graphen_MAIN.csv")
d <- remove_empty(data, which = c("rows", "cols"))

#use only necessary columns
d <- dplyr::select(d, junc_border, gsyll_type, gfoot, gfoot_border, 
                   gfoot_2, gfoot_border_2, WaZ, word_struc, code_neu, letter_rec)

#factorize
d$junc_border <- as.factor(d$junc_border)
d$gsyll_type <- as.factor(d$gsyll_type)
d$gfoot <- as.factor(d$gfoot)
d$gfoot_2 <- as.factor(d$gfoot_2)
d$gfoot_border <- as.factor(d$gfoot_border)
d$gfoot_border_2 <- as.factor(d$gfoot_border_2)
d$word_struc <- as.factor(d$word_struc)
d$WaZ <- as.logical(d$WaZ)
str(d)


#backup a dataset for later analysis of letter shape
d2 <- d

#remove line break separations and last letters
d <- filter(d, !WaZ)
d <- filter(d, word_struc != "fin")
d$WaZ <- NULL
d$word_struc <- NULL
d <- droplevels(d) 

summary(d)



####check if the letters at foot borders are more often separated (dactylus dataset)####

#choose all dactyli
d_gfoot <- filter(d, gfoot == "DAK")

(t_gfoot <- table(d_gfoot$junc_border, d_gfoot$gfoot_border))

#chisq.test
(test <- chisq.test(t_gfoot))  ##significant
assocstats(t_gfoot)
chisq.posthoc.test(t_gfoot)

#save graphics
png("assocplot_fußgrenzeDakt.png")
assocplot(t_gfoot)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungFussgrenzeDak", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####check trochees in the dactylus dataset####

#select only trochees
d_gfoot <- filter(d, gfoot == "TRO")

(t_gfoot <- table(d_gfoot$junc_border, d_gfoot$gfoot_border))

#chisq.test
(test <- chisq.test(t_gfoot))  ##significant
assocstats(t_gfoot)
chisq.posthoc.test(t_gfoot)

#graphic output
png("assocplot_fussgrenzeDakTro.png")
assocplot(t_gfoot)
dev.off()

#store p-value externally to apply bonferrroni-holm later
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungFussgrenzeDakTro", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")



####check if the letters at foot borders are more often separated (only trochee dataset)####
#choose only trochees
d_gfoot_2 <- filter(d, gfoot_2 == "TRO")

(t_gfoot_2 <- table(d_gfoot$junc_border, d_gfoot$gfoot_border_2))

#chisq.test
(test <- chisq.test(t_gfoot_2))  ##significant
assocstats(t_gfoot_2)
chisq.posthoc.test(t_gfoot_2)

#save graphics
png("assocplot_fußgrenzeTro.png")
assocplot(t_gfoot_2)
dev.off()

#check if the letters at foot borders are more often separated (dactylus dataset)
p <- read.csv2("../pvalues.csv")
p <- rbind(p, c("UnterbrechungFussgrenzeTro", test$p.value))
colnames(p) <- c("Test", "p")
write_csv2(p, "../pvalues.csv")


####check e shape within and without reduced syllables and####

#remove upper case and unrecognizable letters
d2 <- filter(d2, letter_rec != "0")
d2 <- filter(d2, !str_detect(code_neu, "99"))

#check correlation
checkLetter("e")
