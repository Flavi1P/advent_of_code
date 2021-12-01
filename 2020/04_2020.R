library(tidyverse)

ipt <- readLines('04-2020.txt')

ipt2 <- c()
t <- ""
for(i in c(1:length(ipt))){
  t <- paste(t, ipt[i])
  if(ipt[i] == ""){
    ipt2 <- c(ipt2, t)
    t <- ''
  }
}

output <- rep(NA, 284)
for(i in c(1:length(ipt2))){
  count <- str_count(ipt2[i], pattern = ":")
  if(count == 8){
    output[i] <- 'valid'
  }
  else if(count == 7 & !any(grep('cid', ipt2[i]))){
    output[i] <- 'valid'
  }
}

ipt3 <- gsub('cid:.{2,3} ', '', ipt2)
ipt3 <- sapply(lapply(strsplit(ipt3, " "), sort),paste,collapse=" ")
ipt3 <- gsub('.{3}:', '', ipt3)
ipt3 <- ipt3[which(output == 'valid')]

ipt3 <- as.tibble(ipt3) %>%  separate(ipt3, 1, into = paste('x', c(1:9), sep = ''), sep = ' ') %>% 
  select(-x1, -x9)

names(ipt3) <- c('byr', 'ecl', 'eyr', 'hcl', 'hgt', 'iyr', 'pid')

result <- filter(ipt3, byr %in%c(1920:2002)) %>% 
  filter(iyr %in% c(2010:2020)) %>% 
  filter(eyr %in% c(2020:2030)) %>% 
  filter(hgt %in% paste(c(150:193), 'cm', sep = '') | hgt %in% paste(c(59:76), 'in', sep = '')) %>% 
  filter(ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'))

correct_hcl <- grep('^#([a-z0-9]{6})$', result$hcl)
result <- result[correct_hcl, ]
corect_pid <- grep('^[0-9]{9}$', result$pid)
result <- result[corect_pid,]

