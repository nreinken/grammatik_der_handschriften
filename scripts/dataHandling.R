#dataHandling.R
#data handling functions
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(janitor)){install.packages("janitor")}
library(tidyverse)

data.loadData <- function(whichColumns = "", letter = NULL, removeWaZ = T, removeWordEnds = F, removeUpperCase = F, removeUnrecognisable = F)
{
 
  #determine the columns to be loaded
  whichColumns_intern <- append(whichColumns, c("letter_rec", "WaZ", "word_struc", "code"))
  whichColumns_intern <- unique(whichColumns_intern)
  
  d <- readr::read_csv2("Graphen_MAIN.csv", col_select = all_of(whichColumns_intern))
  d <- janitor::remove_empty(d, which = c("rows", "cols"))
  
  #keep only needed letters
  if(!is.null(letter))
  {
    d <- filter(d, letter_rec %in% letter)
  }
  
  #convert to factors and integers
  d <- d %>% mutate_if(is.character,as.factor)
  d <- d %>% mutate_if(is.logical,as.factor)
  d <- d %>% mutate_if(is.double,as.integer)
  

  #remove line break separations
  if(removeWaZ)
  {
    d <- filter(d, WaZ != T)
  }
  d$WaZ <- NULL
  
  
  #remove word ends
  if(removeWordEnds)
  {
    d <- filter(d, word_struc != "fin")
  }
  d$word_struc <- NULL
  
  #remove upper case
  if(removeUpperCase)
  {
    d <- filter(d, letter_rec != "0")
  }
  
  #remove unrecognisable letters
  if(removeUnrecognisable)
  {
    d <- filter(d, !str_detect(code, "99"))
  }

  
  #select desired columns
  if(length(whichColumns) > 0)
  {
    d <- dplyr::select(d, any_of(whichColumns))
  }
 
  d <- droplevels(d)
  
  print(glimpse(d, n = 10))
  return(d)
}
