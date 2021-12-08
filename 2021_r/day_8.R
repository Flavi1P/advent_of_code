library(tidyverse)

inp <-  read_delim("~/Documents/aoc/advent_of_code/data/2021/input_8.txt", 
                   delim = "|", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)

solution <- inp %>%
  select(X2) %>% 
  separate(X2, " ", into = c("x1", "x2", "x3", "x4")) %>%
  mutate(row_id = c(1:200)) %>% 
  pivot_longer(1:4, names_to = "output", values_to = "string") %>% 
  mutate("test" = str_count(string, "(^[a-z]{4}$)|(^[a-z]{2}$)|(^[a-z]{3}$)|(^[a-z]{7}$)")) %>% 
  summarise(test = sum(test))
solution

str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

solution2 <- inp %>%
  select(X1) %>% 
  separate(X1, " ", into = c(paste("X", c(1:10), sep = ""))) %>%
  mutate(row_id = c(1:200)) %>% 
  pivot_longer(1:10, names_to = "output", values_to = "string") %>% 
  mutate("nchar" = str_count(string, pattern = "[a-z]")) %>% 
  mutate("string" = str_arrange(string)) %>% 
  mutate("numb" = case_when(nchar == 2 ~ "x1",
                            nchar == 4 ~ "x4",
                            nchar == 3 ~ "x7",
                            nchar == 7 ~ "x8",
                            nchar == 5 ~ "x325",
                            nchar == 6 ~ "x960"),
         "find" = case_when(str_count(numb) == 2 ~ numb,
                            str_count(numb) != 2 ~ "no"),
         "charlist" = strsplit(string, "")) %>% 
  pivot_wider(c(row_id, charlist, find), names_from = find, values_from = charlist) %>% 
  unnest(2:6) %>% 
  rowwise() %>% 
  mutate("discover" = case_when(
    length(no) == 6 & length(which(x4 %in% no)) == 4 ~ 9,
    length(no) == 6 & length(which(x4 %in% no)) == 3 & length(which(x1 %in% no)) == 2 ~ 0,
    length(no) == 6 & length(which(x1 %in% no)) == 1 ~ 6,
    length(no) == 5 & length(which(x1 %in% no)) == 2 ~ 3,
    length(no) == 5 & length(which(x4 %in% no)) == 2 ~ 2,
    length(no) == 5 & length(which(x4 %in% no)) == 3 & length(which(x1 %in% no)) == 1 ~ 5)) %>% 
  pivot_wider(1:7, names_from = discover, values_from = no) %>% 
  pivot_longer(2:11, names_to = "number", values_to = "seq") %>% 
  rowwise() %>% 
  mutate(number = str_extract(number, "[0-9]"),
         string = paste(seq, collapse = ""))

table <- inp %>% mutate(row_id = c(1:200)) %>% 
  left_join(solution2) %>% 
  separate(X2, " ", into = c("out1", "out2", "out3", "out4")) %>% 
  pivot_longer(2:5, names_to = "position", values_to = "str") %>% 
  mutate(str = str_arrange(str)) %>% 
  filter(str == string) %>% 
  select(row_id, position, number) %>% 
  pivot_wider(c(1,2,3), names_from = position, values_from =number) %>% 
  mutate(final = as.numeric(paste(out1,out2,out3,out4, sep = "")))
sum(table$final)  
