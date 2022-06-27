#downsampling.R
#choose a subset from all abitur essays of 2003-2013 from the GraphVar-corpus
#© Niklas Reinken, July 2020


#chooses a downsample from all abitur essay and checks if it's representative for the whole sample (it should be)

#load libraries
library(dplyr);

#load data
d <- read.table("Gesamtes Korpus/Abikorpus_Meta_Gesamtliste.csv", na.string = "", header = T, sep = ";", dec=",", encoding = "utf-8");

str(d);

#correcting variable types, Jahr needs to be an integer for now so we can use the < operator later
d$Jahr <- as.integer(d$Jahr);
d$Punkte <- as.numeric(d$Punkte);
d$Note <- as.numeric(d$Note);
d$Tokens <- as.numeric(d$Tokens);
d$Types <- as.numeric(d$Types);
d$Type.Token.Ratio <- as.numeric(d$Type.Token.Ratio);
d$Kommentar <- NULL;


#subsetting unusable data
d <- subset(d, Jahr>=1963);
d <- subset(d, Lehrer!="?");
d <- subset(d, Geschlecht!="X");
d <- na.omit(d);

#now Jahr can be a factor
d$Jahr <- as.factor(d$Jahr);

d <- droplevels(d);

#starting the loop
run = T;
while(run)
{
  #setting default value
  d$drin <- F;

  #choose random cases
  s <- d %>% sample_n(200);

  #flagging sampled cases
  s$drin <- T;

  #combining them again
  dfull <- rbind(d, s);

  run = F;
  
  #check if numerical values are equally distributed and rerun the loop if they are
  res1 <- t.test(dfull$Punkte~dfull$drin);
  res2 <- t.test(dfull$Note~dfull$drin);
  res3 <- t.test(dfull$Tokens~dfull$drin);
  res4 <- t.test(dfull$Types~dfull$drin);
  res5 <- t.test(dfull$Type.Token.Ratio~dfull$drin);

  #check if the categorical data is equally distributed
  res6 <- chisq.test(table(dfull$Jahr,dfull$drin));
  res7 <- chisq.test(table(dfull$Fach,dfull$drin));
  res8 <- chisq.test(table(dfull$Geschlecht,dfull$drin));
  
  ress <- c(res1$p.value, res2$p.value, res3$p.value, res4$p.value, res5$p.value, res6$p.value, res7$p.value, res8$p.value);
  minres <- min(ress);
  if(minres <= 0.8)
  {
    run = T;
  }
 #save list to a new file
write.csv2(s, "Handschriftenkorpus_Gesamtliste.csv");
}
