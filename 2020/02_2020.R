library(tidyverse)
ipt <- read.delim( "02-2020.txt",  header = FALSE )

ipt.2 <- separate(ipt, V1, into = c('numb', 'letter', 'pwd'), sep = " ") %>% 
  mutate(letter = substr(letter, 1, 1)) %>% 
  separate(numb, into = c('min', 'max'), sep = '-') %>% 
  mutate(count = str_count(pwd, letter)) %>% 
  filter(count >= as.numeric(min) & count <= as.numeric(max))

ipt.3 <- separate(ipt, V1, into = c('numb', 'letter', 'pwd'), sep = " ") %>% 
  mutate(letter = substr(letter, 1, 1)) %>% 
  separate(numb, into = c('min', 'max'), sep = '-') %>% 
  mutate(l1 = substr(pwd, as.numeric(min), as.numeric(min)),
         l2 = substr(pwd, as.numeric(max), as.numeric(max)),
         l3 = paste(l1,l2),
         matches = str_count(l3, letter)) %>% 
  filter(matches == 1)
table(result)
