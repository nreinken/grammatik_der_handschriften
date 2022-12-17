#contingencyTests.R
#flexible functions to test the automatically test the contingency of letter form, junction and grammatical features
#based on scripts by Niklas Reinken, July 2021
#version 1, December 2022

cont_test <- function(data)
{
  t <- table(data)
  print(t)
  
  #determine wether to use fisher's exact test or chi square
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
  return(test)
}
