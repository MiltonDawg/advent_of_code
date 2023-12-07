library(readxl)
library(openxlsx)
library(tidyverse)
library(conflicted)
conflict_prefer_all('readxl', quiet = T)
conflict_prefer_all('dplyr', quiet = T)
conflict_prefer_all('tidyr', quiet = T)
setwd('U://workplace/advent_code')

df <- read_csv('day2.csv')

red_max <- 12
  
blue_max <-  14 

green_max <- 13
  

df1 <- df %>% 
  separate_wider_delim(everything(), 
                       ';', 
                       names = c('Game','round2','round3','round4','round5','round6'),
                       too_few = 'align_start') %>% 
  separate_wider_delim(cols = 'Game',
                       ':',
                       names = c('Game','round1')) %>% 
  pivot_longer(cols = starts_with('round'), names_to = 'round', names_prefix = 'round', values_to = 'round_string') %>% 
  filter(!is.na(round_string)) %>% 
  mutate(Game = gsub('Game ', '', Game)) %>% 
  mutate(red = replace_na(as.numeric(str_extract(round_string, '^*(\\d+) red*', group = 1)),0),
         blue = replace_na(as.numeric(str_extract(round_string, '^*(\\d+) blue*', group = 1)),0),
         green = replace_na(as.numeric(str_extract(round_string, '^*(\\d+) green*', group = 1)),0)) %>% 
  mutate(round_check = red <= red_max & blue <= blue_max & green <= green_max) %>% 
  summarise(n = mean(round_check),
            .by = Game) %>% 
  filter(n == 1) %>% 
  distinct(Game) %>% 
  summarise(sum = sum(as.numeric(Game)))

df2 <- df %>% 
  separate_wider_delim(everything(), 
                       ';', 
                       names = c('Game','round2','round3','round4','round5','round6'),
                       too_few = 'align_start') %>% 
  separate_wider_delim(cols = 'Game',
                       ':',
                       names = c('Game','round1')) %>% 
  pivot_longer(cols = starts_with('round'), names_to = 'round', names_prefix = 'round', values_to = 'round_string') %>% 
  filter(!is.na(round_string)) %>% 
  mutate(Game = gsub('Game ', '', Game)) %>% 
  mutate(red = replace_na(as.numeric(str_extract(round_string, '^*(\\d+) red*', group = 1)),0),
         blue = replace_na(as.numeric(str_extract(round_string, '^*(\\d+) blue*', group = 1)),0),
         green = replace_na(as.numeric(str_extract(round_string, '^*(\\d+) green*', group = 1)),0)) %>% 
  mutate(max_red = max(red),
         max_blue = max(blue),
         max_green = max(green),
         .by = Game) %>% 
  distinct(Game, .keep_all = T) %>% 
  mutate(n = max_red * max_blue * max_green) %>% 
  summarise(f = sum(n))

