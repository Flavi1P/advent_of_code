library(tidyverse)

dat <- readLines("data/2021/input_2.txt") %>% as_tibble() %>% 
  separate(1, sep = "", into = paste("x", seq(1,13), sep = "")) %>% 
  select(-1)

dat <-   as_tibble(lapply(dat ,as.numeric))

#sum all values and check if the value is superior to the half of the legnth of the input (500). If so, 1 is dominent.

dat2 <- dat %>% summarise_all(sum) %>% mutate(across(, ~.x - 500)) %>% 
  pivot_longer(1:12, names_to = "psotion", values_to = "value") %>% 
  mutate(gamma = case_when(value < 0 ~ 0,
                           value > 0 ~ 1),
         epsilon = case_when(value < 0 ~ 1,
                             value > 0 ~ 0))

gamma <- paste(dat2$gamma, collapse = "")
epsilon <- paste(dat2$epsilon, collapse = "")

gamma <- strtoi(gamma, base = 2)
epsilon <- strtoi(epsilon, base = 2)

gamma * epsilon


# part 2 ------------------------------------------------------------------

t <- dat
i <- 1
while(nrow(t) > 1){
  size = nrow(t) / 2
  test <- sum(t[,i]) - size
  if(test < 0){
    t <- t[t[,i] == 0,]
  }
  if(test >=0){
    t <- t[t[,i] == 1,]
  }
  i <- i + 1
}

dat3 <- t %>% pivot_longer(1:12, names_to = "position", values_to = "gamma")

gamma <- paste(dat3$gamma, collapse = "")
gamma <- strtoi(gamma, base = 2)

t <- dat
i <- 1
while(nrow(t) > 1){
  size = nrow(t) / 2
  test <- sum(t[,i]) - size
  if(test >= 0){
    t <- t[t[,i] == 0,]
  }
  if(test < 0){
    t <- t[t[,i] == 1,]
  }
  i <- i + 1
}

dat4 <- t %>% pivot_longer(1:12, names_to = "position", values_to = "epsilon") 
  
epsilon <- paste(dat4$epsilon, collapse = "")
epsilon <- strtoi(epsilon, base = 2)

gamma * epsilon
