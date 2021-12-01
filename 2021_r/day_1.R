library(tidyverse)
input_1 <- readLines("data/2021/input_1.txt") %>% as.numeric()

dat <- tibble(input_1)

dat %>% mutate(test = case_when(
  lag(.) < . ~ "increase",
  lag(.) > . ~ "decrease",
  TRUE ~ "N/A")) %>% 
  group_by(test) %>% 
  tally()

dat %>% 
  mutate(measurement_sum = . + lead(., n = 1) + lead(., n = 2)) %>% 
  mutate(measurement = case_when(
    lag(measurement_sum) < measurement_sum ~ "increased",
    lag(measurement_sum) > measurement_sum ~ "decreased",
    TRUE ~ "N/A")) %>% 
  group_by(measurement) %>% 
  tally()
