#padj.R
#apply bonferroni-holm-correction to all p-values
#© Niklas Reinken, July 2021
options(scipen = 999)

#load library
library(tidyverse)

#load p-values from .csv-file
p <- read_csv2("pvalues.csv")

#data casting
p$p <- as.numeric(p$p)

#remove duplicates
p <- unique(p)
write_csv2(p, "pvalues.csv", append = F)

#apply correction
p$p.adj <- round(p.adjust(p$p, method = "holm"),3)
p <- mutate(p, sig = ifelse(p.adj < 0.05, "sig", ""))
