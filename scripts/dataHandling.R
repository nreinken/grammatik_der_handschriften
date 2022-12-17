#dataHandling.R
#data handling functions
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(cli)){install.packages("cli")}
if(!require(readr)){install.packages("readr")}
library(cli)
library(readr)

data.loadData <- function()
{
  data <- readr::read_csv2("../Graphen_MAIN.csv")
  return(data)
}
