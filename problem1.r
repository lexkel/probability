  
  #============================================
  # Fifty challenging problems in probability
  #
  # Problem 1: The sock drawer
  #
  #============================================
  
  library(tidyverse)

  set.seed(42)
  
  r <- 5
  b <- 2
  
  two.red.socks <- function(r, b) {
    p1 <- r / (r + b)             # Probability first sock is red
    p2 <- (r - 1) / ((r-1) + b)   # Probability second sock is red 
    p3 <- p1 * p2                 # Probability both socks are red
    p3
  }
  
  # Simulate the probabilities for all red and black socks number combinations up to 100
  df <- crossing(r = seq(1,100), b = seq(1,100)) %>% 
          mutate(two.red.socks = socks(r = r, b = b))

  df %>% filter(two.red.socks == 0.5)
  df %>% filter(two.red.socks > 0.49 & two.red.socks < 0.51)
  
  ggplot() +
    geom_histogram(data = df, aes(x = both.red), binwidth = 0.001) + 
    geom_vline(xintercept = 0.5, colour = "red") +
    theme_minimal() +
    labs(title = "Probability that both socks will be red when randonmly drawn",
         x = NULL, 
         y = NULL)
  