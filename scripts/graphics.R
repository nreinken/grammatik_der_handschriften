#contingencyTests.R
#functions for graphical output and diagrams
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

plot_assoc <- function(test, x.title = "", y.title ="", eps = T, png = T)
{
  resids <- data.frame(test$residuals)
  resids$color <- ifelse(resids$Freq < 0, "negative", "positive")
  resids$count <- data.frame(test$observed)$Freq
  
  ggplot(resids, aes(fill=color, y=Freq, x=resids[,2])) +
    geom_bar(position = "dodge", stat = "identity", width=((resids$count)/sum(resids$count))) +
    scale_fill_grey() +
    facet_wrap(~resids[,1], dir = "v", strip.position = "left") +
    theme_minimal() +
    theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
    scale_y_continuous(position = "right") +
    xlab("") +
    ylab("Residuals") +
    geom_hline(aes(yintercept = 0))
  
  if(eps)
  {
     ggsave(paste0("graphs/assoc_", x.title, "_", y.title, ".eps"), width = 3, height = 3)
  }
  if(png)
  {
    ggsave(paste0("graphs/assoc_", x.title, "_", y.title, ".png"), width = 3, height = 3)
  }
 
}
