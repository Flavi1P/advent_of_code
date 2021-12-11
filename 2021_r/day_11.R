library(tidyverse)

input <- readLines("data/2021/input_11.txt") %>% 
  strsplit("") %>% 
  unlist() %>% 
  as.numeric()
input <- t(matrix(input, ncol = 10, nrow =10))

get_position <- function(position){
  pos_new <- c()
  for(i in position){
    if(i %% 10 == 0){
      pos2 <- c(i - 1, i - 10, i - 11, i + 10, i + 9)
    }
    if(i %% 10 == 1 | i == 1){
      pos2 <- c(i + 1, i - 10, i - 9, i + 10, i + 11)
    }
    if(i %% 10 %in% c(2:9) | i %in% c(2:9)){
      pos2 <- c(i - 1, i - 10, i - 9, i - 11, i + 9, i + 1, i + 10, i + 11)
    }
    pos_new <- c(pos_new, pos2)
  }
  pos_new <- pos_new[pos_new > 0 & pos_new < 101]
  return(pos_new)
}

day <- function(data){
  data <- data + 1
  while(any(data > 9 & data < 10000)){
    pos <- which(data > 9 & data < 10000)
    data[pos] <- 10000
    for(i in pos){
      j <- get_position(i)
      j <- j[!j %in% pos]
      data[j] <- data[j] +1
    }
  }
  data[which(data >= 10000)] <- 0
  return(data)
}

count <- 0
new_day <- input
for(i in 1:100){
  new_day <- day(new_day)
  count <- count + length(which(new_day == 0))
}


# solution 2 --------------------------------------------------------------

flashes <- input
day_numb <- 0

while(!all(flashes == 0)){
  flashes <- day(flashes)
  day_numb <- day_numb + 1
}
