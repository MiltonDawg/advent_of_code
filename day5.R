library(readxl)
library(openxlsx)
library(janitor)
library(haven)
library(scales)
library(tidyverse)
library(infixit)
library(conflicted)
conflict_prefer_all('readxl', quiet = T)
conflict_prefer_all('dplyr', quiet = T)
conflict_prefer_all('tidyr', quiet = T)
setwd('U://workplace/advent_code')

seed <- read_xlsx('day5.xlsx', sheet = 'seeds', col_names = 'seed')
seed_soil <- read_xlsx('day5.xlsx', sheet = 'seed_soil', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
soil_fert <- read_xlsx('day5.xlsx', sheet = 'soil_fert', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
fert_water <- read_xlsx('day5.xlsx', sheet = 'fert_water', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
water_light <- read_xlsx('day5.xlsx', sheet = 'water_light', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
light_temp <- read_xlsx('day5.xlsx', sheet = 'light_temp', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
temp_humid <- read_xlsx('day5.xlsx', sheet = 'temp_humid', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
humid_local <- read_xlsx('day5.xlsx', sheet = 'humid_local', col_names = c('dest_strt','src_start','range')) %>% mutate(diff = rowSums(cbind(dest_strt, -1 * src_start)))
options(scipen = 999)
seed[1,'seed']

local <- tibble(NULL)

for (i in 1:nrow(seed['seed'])) {

seed_soil1 <- seed_soil %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(seed[i,'seed']) >= src_start ,
         check2 = pull(seed[i,'seed']) <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(soil = if_else(max(check3 * (pull(seed[i,'seed'])  + diff)) == 0, pull(seed[i,'seed']), max(check3 * (pull(seed[i,'seed'])  + diff)))) %>% 
  slice(1)

soil_fert1 <- soil_fert %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(seed_soil1, soil)   >= src_start ,
         check2 = pull(seed_soil1, soil)   <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(fert = if_else(max(check3 * (pull(seed_soil1, soil)  + diff)) == 0, pull(seed_soil1, soil), max(check3 * (pull(seed_soil1, soil) + diff)))) %>% 
  slice(1)

fert_water1 <- fert_water %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(soil_fert1, fert)   >= src_start ,
         check2 = pull(soil_fert1, fert)   <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(water = if_else(max(check3 * (pull(soil_fert1, fert)  + diff)) == 0, pull(soil_fert1, fert), max(check3 * (pull(soil_fert1, fert) + diff)))) %>% 
  slice(1)

water_light1 <- water_light %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(fert_water1, water)   >= src_start ,
         check2 = pull(fert_water1, water)   <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(light = if_else(max(check3 * (pull(fert_water1, water)  + diff)) == 0, pull(fert_water1, water), max(check3 * (pull(fert_water1, water) + diff)))) %>% 
  slice(1)

light_temp1 <- light_temp %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(water_light1, light)   >= src_start ,
         check2 = pull(water_light1, light)   <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(temp = if_else(max(check3 * (pull(water_light1, light)  + diff)) == 0, pull(water_light1, light), max(check3 * (pull(water_light1, light) + diff)))) %>% 
  slice(1)

temp_humid1 <- temp_humid %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(light_temp1, temp)   >= src_start ,
         check2 = pull(light_temp1, temp)   <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(humid = if_else(max(check3 * (pull(light_temp1, temp)  + diff)) == 0, pull(light_temp1, temp), max(check3 * (pull(light_temp1, temp)  + diff)))) %>% 
  slice(1)

humid_local1 <- humid_local %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         check1 = pull(temp_humid1, humid)   >= src_start ,
         check2 = pull(temp_humid1, humid)   <= src_end,
         check3 = check1 == T & check2 == T) %>% 
  mutate(local = if_else(max(check3 * (pull(temp_humid1, humid)  + diff)) == 0, pull(temp_humid1, humid), max(check3 * (pull(temp_humid1, humid)  + diff)))) %>% 
  slice(1)

local <- local %>% 
  bind_rows(humid_local1[1,'local'])
  
}
min(local)

humid_local2 <- humid_local %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1) %>% 
  slice_min(dest_strt,n=7) %>% 
  slice_max(dest_strt, n=1)

temp_humid2 <- temp_humid %>% 
  mutate(src_end = rowSums(cbind(src_start, range)) -1,
         dest_end = rowSums(cbind(dest_strt, range)) -1) %>% 
  mutate(check1 = pull(humid_local2[1,'src_start']) >= src_start & pull(humid_local2[1,'src_start']) <= src_end) %>% 
  arrange(dest_strt)
  
