library(tidyverse)
library(mgcv)

inp <- readLines("data/2021/input_6.txt") %>% strsplit(",")
inp <- as.numeric(inp[[1]])

baby <- function(data){
  n_baby <- length(data[data == 0])
  new_data <- c(data, rep(9, n_baby))
  new_data[new_data == 0] <- 7
  return(new_data)
}


for(i in c(1:80)){
  inp <- baby(inp)
  inp <- inp - 1
  }

length(inp)

input_agg <- table(inp) %>% as.vector() %>% unname()
# Note all inputs are in 1-5, so need to adjust a bit.
input_agg <- c(0, input_agg, 0, 0, 0)

advance_day <- function(input){
  out <- c(input[-1], input[1])
  out[7] <- out[7] + input[1]
  return(out)
} 

in_p2 <- input_agg

for (i in 1:256){
  in_p2 <- advance_day(in_p2)
}

options(scipen=999)
(sum(in_p2))
