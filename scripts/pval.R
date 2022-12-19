#pVal.R
#automatically perfom bonferoni-holm-tests to p-values
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

pval <- function (pvalue, title)
{

  #create tibble for p-values, if it doesn't exist
  if(!exists("pvals"))
  {
    pvals = dplyr::tibble(id = "", pvalue = 0)
  }
  
  #store p-value
  pvals <- add_row(pvals, tibble_row(id = title, pvalue = pvalue))
  
  
  #remove duplicates and empty rows
  pvals <- distinct(pvals)
  pvals <- filter(pvals, id != "") 
  
  #apply bonferoni-holm correction
  pvals$padj <- round(p.adjust(pvals$pvalue, method = "holm"),5)
  
  #print pvals for testing, TODO: remove later
  print(pvals)
  
  #return adjusted p-value
  case <- filter(pvals, id == title)
  return (case$padj)
}
