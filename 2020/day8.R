library(tidyverse)

inpt <- readLines('inpt_day8.txt') %>% as.tibble()

inpt <- separate(inpt, 1, into = c('com', 'value'), sep = ' ')
inpt$value <- as.numeric(inpt$value)
vec <- c()
acc <- 0
t <- 1
while(!t %in% vec){
  vec <- c(vec, t)
  comt <- inpt[t, 1]
  valuet <- as.numeric(inpt[t, 2])
  if(comt == 'acc'){
    acc <- acc + valuet
    t <- t+1
  }
  if(comt == 'nop'){
    t <- t+1
  }
  if(comt == 'jmp'){
    t <- t + valuet
  }
}

#part 2

run <- function(commands){
  inpt2 <- commands
  vec <- c()
  acc <- 0
  t <- 1
  while(!t %in% vec){
  vec <- c(vec, t)
  comt <- inpt2[t, 1]
  valuet <- as.numeric(inpt2[t, 2])
  if(comt == 'acc'){
    acc <- acc + valuet
    t <- t+1
  }
  if(comt == 'nop'){
    t <- t+1
  }
  if(comt == 'jmp'){
    t <- t + valuet
  }
  if(t >= 656){
    t <- 1
  }
  }
  if(t < length(inpt$com)){
    test <- 'infinit loop'
  }
  if(t >= length(inpt$com)){
    test <- 'complete loop'
  }
  return(c(test = test,
           acc = acc))
}

i <- 0
test <- 'infinit loop'
while(test == 'infinit loop' & i < 656){
  i <- i + 1
  t_inpt <- inpt
  if(inpt[i,1] == 'jmp'){
    t_inpt[i,1] <- 'nop'
    test <- run(t_inpt)[1]
  }
  if(inpt[i,1] == 'nop'){
    t_inpt[i,1] <- 'jmp'
    test <- run(t_inpt)[1]
  }
}

test <- run(t_inpt)[1]
