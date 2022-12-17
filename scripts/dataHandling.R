#dataHandling.R
#data handling functions
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(janitor)){install.packages("janitor")}
library(tidyverse)

data.loadData <- function(whichColumns = "", removeWaZ = T, removeWordEnds = F)
{
  d <- readr::read_csv2("Graphen_MAIN.csv")
  d <- janitor::remove_empty(d, which = c("rows", "cols"))
  
  #convert to factors and integers
  d <- d %>% mutate_if(is.character,as.factor)
  d <- d %>% mutate_if(is.double,as.integer)
  
  
  #remove line break separations
  if(removeWaZ)
  {
    d <- filter(d, d$WaZ != T)
  }
  d$WaZ <- NULL
  
  
  #remove word ends
  if(removeWordEnds)
  {
    d <- filter(d, d$word_struc != "fin")
  }
  d$word_struc <- NULL
  
  #select desired columns
  if(length(whichColumns) > 0)
  {
    d <- dplyr::select(d, any_of(whichColumns))
  }
 
  d <- droplevels(d)
  
  print(glimpse(d, n = 10))
  return(d)
}
