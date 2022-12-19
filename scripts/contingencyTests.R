#contingencyTests.R
#flexible functions to test the automatically test the contingency of letter form, junction and grammatical features
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

source("scripts/pval.R")
source("scripts/graphics.R")

if(!require(vcd)){install.packages("vcd")}
if(!require(chisq.posthoc.test)){install.packages("chisq.posthoc.test")}

cont_test <- function(data, x.title = "", y.title = "")
{
  title = paste0(x.title, "X", y.title)
  t <- table(data)
  print(t)
  
  #determine whether to use fisher's exact test or chi square
  pre_test <- chisq.test(t)
  fisher <- if(min(pre_test$expected) < 5) T else F

  #calculate contingency tests
  if(fisher == T)
  {
    print("Fisher-Test")
    print(test <- fisher.test(t, simulate.p.value = T))
  }
  else
  {
    print("Chi-Quadrat-Test")
    print(test <- chisq.test(t))
  }
  
  #return if not significant
  if(test$p.value > 0.05)
  {
    print("### not significant ###")
    return()
  }
 
  #apply bonferoni-holm correction
  padj <- pval(pval = test$p.value, title = title)
  print(paste0("p.adj = ", padj))
  if(padj >= 0.05)
  {
    print("### adjusted p-value is not significant ###")
  }
  else
  {
    print("adjusted p-value is significant")
    
    #run post hocs
    print(vcd::assocstats(t))
    print(chisq.posthoc.test::chisq.posthoc.test(t))
    
    #create graphical output
    plot_assoc(test, x.title = x.title, y.title = y.title)
  }

  #clean up and return
  data <- NULL
  t <- NULL
  test <- NULL
  gc(verbose = F)
}
