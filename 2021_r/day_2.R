library(tidyverse)

inp <- readLines("data/2021/input_2.txt") %>% as.tibble()

data <- inp %>% separate(value, sep = " ", into = c("instruction", "value")) %>%
  mutate(value = as.numeric(value)) %>% 
  group_by(instruction) %>% 
  summarise(sum = sum(value))

data$sum[data$instruction == "forward"] * (data$sum[data$instruction == "down"] - data$sum[data$instruction == "up"])


# part 2 ------------------------------------------------------------------

forward <-  data$sum[data$instruction == "forward"]

data <- inp %>%
  separate(value, sep = " ", into = c("instruction", "value")) %>%
  mutate(value = as.numeric(value))

depth <- 0
aim <- 0

for(i in 1:length(data$value)){
  inst <- data$instruction[i]
  val <- data$value[i]
  if(inst == "forward"){
    depth <- depth + aim * val
  }
  if(inst == "down"){
    aim <-  aim + val
  }
  if(inst == "up"){
    aim <- aim - val
  }
}

depth * forward
