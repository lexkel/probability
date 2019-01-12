  
  #============================================
  # Fifty challenging problems in probability
  #
  # Problem 2: Successive wins
  #
  #============================================
  
  library(tidyverse)

  set.seed(42)
  
  # Elmers's probability of beating the father is higher than beating the champion
  prob.father <- 0.55
  prob.champion <- 0.45
  
  # Best of simulations
  n <- 10000
  
  father.first <- tibble(Trial = seq(1,n),
                         First.match = rbinom(n, 1, prob.father),
                         Second.match = rbinom(n, 1, prob.champion),
                         Third.match = rbinom(n, 1, prob.father)) %>%
                      mutate(Win.two.in.row = case_when(First.match + Second.match == 2 ~ "Prize",
                                                        Second.match + Third.match == 2 ~ "Prize",
                                                        TRUE ~ "No prize")) 
  
  champion.first <- tibble(Trial = seq(1,n),
                           First.match = rbinom(n, 1, prob.champion),
                           Second.match = rbinom(n, 1, prob.father),
                           Third.match = rbinom(n, 1, prob.champion)) %>%
                      mutate(Win.two.in.row = case_when(First.match + Second.match == 2 ~ "Prize",
                                                        Second.match + Third.match == 2 ~ "Prize",
                                                        TRUE ~ "No prize")) 

  father.first %>% group_by(Win.two.in.row) %>% summarise(n())
  champion.first %>% group_by(Win.two.in.row) %>% summarise(n())  
  
  # Elmer should choose to play champion, father, champion to maximise his liklihood of getting the prize!!!
                 
  