#pVal.R
#automatically perform bonferoni-holm-tests to p-values
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

pval <- function (pvalue, title)
{
  #create tibble for p-values in the global environment, if it doesn't exist
  if(!exists("pvals"))
  {
    assign("pvals", dplyr::tibble(id = "", pvalue = 0), envir = globalenv())
  }
  
  #store p-value
  pvals.loc <- pvals
  pvals.loc <- add_row(pvals.loc, tibble_row(id = title, pvalue = pvalue))
  
  
  #remove duplicates and empty rows
  pvals.loc <- filter(pvals.loc, id != "") 
  pvals.loc <- distinct(pvals.loc)
  
  #apply bonferoni-holm correction
  pvals.loc$padj <- round(p.adjust(pvals.loc$pvalue, method = "holm"),5)
  
  #print pvals for testing, TODO: remove later
  #print(pvals.loc)
  
  #store pvals in the global environment to use it in the next call of this function
  assign("pvals", pvals.loc, envir = globalenv())
  
  #return adjusted p-value
  case <- filter(pvals, id == title)
  return (case$padj)
}
