library(officedown)
library(officer)
library(extrafont)
library(readxl)
library(openxlsx)
library(janitor)
library(haven)
library(scales)
library(tidyverse)
library(conflicted)
conflict_prefer_all('readxl', quiet = T)
conflict_prefer_all('dplyr', quiet = T)
conflict_prefer_all('tidyr', quiet = T)
setwd('U://workplace/advent_code')

df <- read_csv('day3.csv', col_names = F)

df1 <- df %>% 
  separate_wider_delim(X1, ':', names = c('index','details')) %>% 
  separate_wider_delim(details, '|', names = c('winning','played')) %>% 
  mutate(winning = str_squish(winning),
         played = str_squish(played)) %>% 
  separate_wider_delim(winning, ' ', names = paste0('winning',seq(1,10,1))) %>% 
  pivot_longer(cols = starts_with('winning'), names_to = 'iteration_winning', values_to = 'winning', names_prefix = 'winning') %>% 
  separate_wider_delim(played, ' ', names = paste0('played',seq(1,25,1))) %>% 
  pivot_longer(cols = starts_with('played'), names_to = 'iteration_played', values_to = 'played', names_prefix = 'played') %>% 
  filter(winning == played) %>% 
  count(index) 

df1%>% 
  mutate(number = 2^(n-1)) %>% 
  summarise(n = sum(number))

df1 %>% 
  mutate(number = 2^(n-1)) %>% 
  mutate(card = as.numeric(str_remove(str_squish(index), 'Card '))) %>% 
  select(-index)


















