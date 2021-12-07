library(tidyverse)

inp <- readLines("data/2021/input_7.txt") %>% strsplit(",") %>%  unlist() %>% as.numeric()

test_it <- function(data, num){
  new <- sum(abs(data - num))
  return(new)
}

try <- data.frame("input" = inp) %>%
  rowwise() %>% 
  mutate(fuel = test_it(inp, input))

min(try$fuel)

test_it_2 <- function(data, num){
  new <- abs(data - num)
  new_2 <- (new * new + new)/2 # arithmetic function equivalent of n + n-1 + n-2....
  new_2 <- sum(new_2)
  return(new_2)
}

try_2 <- data.frame("input" = seq(0, max(inp), 1)) %>% #argh the ne position is not one of the position in the input
  rowwise() %>% 
  mutate(fuel = test_it_2(inp, input))

min(try_2$fuel)
