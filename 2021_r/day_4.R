library(tidyverse)

table <- read_table2("data/2021/input_3.txt", 
                     col_names = FALSE, skip = 1) %>% 
  mutate(ntable = sort(rep(1:100, 5)))

instruction <- readLines("Data/2021/input_3.txt")[1] %>% 
  strsplit(., ",") 

instruction <- as.numeric(instruction[[1]])

check_col <- function(data){
  test <- data %>%
    group_by(ntable) %>% 
    summarise_all(sum) %>% 
    pivot_longer(2:6, names_to = "col", values_to = "check")

  if(-5 %in% test$check){
    sol <- filter(test, check == -5) %>% pull(ntable)
    return(sol)
  }
  else{
    return(0)
  }
}


check_row <- function(data){
  test <- data %>%
    mutate(check = rowSums(.[1:5])) 
  if(-5 %in% test$check){
    sol <- filter(test, check == -5) %>% pull(ntable)
    return(sol)
  }
  else{
    return(0)
  }
}

for(i in instruction){
  table[,1:5][table[,1:5] == i] <- -1
  test_row <- check_row(table)
  test_col <- check_col(table)

  if(test_row != 0){
    return(test_row)
  }
  if(test_col != 0){
    return(test_col)
  }
}



final <- read_table2("data/2021/input_3.txt", 
                     col_names = FALSE, skip = 1) %>% 
  mutate(ntable = sort(rep(1:100, 5)),
         nrow = seq(1,500, 1)) %>% 
  filter(ntable == 64) %>% 
  pivot_longer(1:5, names_to = "count") %>% 
  pull(value)


numsel <- instruction[1:which(instruction == i)]
unmarked <- sum(final[!final %in% numsel])

unmarked * i


# part 2 ------------------------------------------------------------------

table2 <- read_table2("data/2021/input_3.txt", 
                      col_names = FALSE, skip = 1) %>% 
  mutate(ntable = sort(rep(1:100, 5)))

for(i in instruction){
  table2[,1:5][table2[,1:5] == i] <- -1
  test_row <- check_row(table2)
  test_col <- check_col(table2)

  table2 <- filter(table2,! ntable %in% test_row)
  table2 <- filter(table2,! ntable %in% test_col)
  if(nrow(table2) == 5){
    final_table <- table2
    final_number <- i
  }
}


result <- final_table %>% 
  pivot_longer(1:5, names_to = "count") %>% 
  filter(value != -1) %>%
  pull(value) %>% 
  sum(.)

multiply <- instruction[which(instruction == final_number) + 1]

(result - 46) * multiply

