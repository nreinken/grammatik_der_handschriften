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

plot_coefs <- function(coefs, name, eps = T, png = T)
{
  ggplot(data = coefs, aes(x = reorder(`rownames(coefs)`, coefs), y = coefs)) +
    geom_point(stat = "identity", size = 5, fill = "white", color ="#222222", shape = 16) +
    geom_segment(aes(y = 0, x = `rownames(coefs)`, yend = coefs, xend = `rownames(coefs)`), color = "#222222") +
    geom_text(aes(y = pos, label = str_c(`rownames(coefs)`,": ", coefs)) ,color = "#222222", size = 3) +
    geom_segment(aes(x = length(`rownames(coefs)`)+1, y = 0, yend = 0, xend = 0), color = "black") +
    coord_flip() +
    labs(y = "Coefficients", x = "") +
    theme_minimal()+
    theme(axis.text.y = element_blank()) +
    ylim(-3,3)

  if(eps)
  {
    ggsave(paste0("graphs/coefs_", name, ".eps"), width = 4, height = 8)
  }
  if(png)
  {
    ggsave(paste0("graphs/coefs_", name, ".png"), width = 4, height = 8)
  }
}
