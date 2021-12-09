library(tidyverse)
library(here)
library(janitor)
library(lubridate)

wrapped <- df %>% 
  select(date, time, error, 
         Brian, Carly, David, Emi, Jeff, Kelsey, Shannon) %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2021) %>% 
  select(-year)
  
freq_first_last <- wrapped %>% 
  pivot_longer(`Brian`:`Shannon`, names_to = "name", values_to = "order") %>% 
  mutate(time = factor(time, levels = c("Standup", "Sitdown"))) %>% 
  filter(!is.na(order)) %>% 
  group_by(date, time) %>% 
  mutate(last_position = max(order)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarize(number_present = length(order),
            number_absent = 439 - number_present,
            number_last = length(order[order == last_position]),
            number_first = length(order[order == 1])) %>%
  mutate(freq_last = round((number_last/number_present)*100, 1),
         freq_first = round((number_first/number_present)*100, 1),
         freq_present = round(number_present/439*100, 1),
         freq_absent = 100 - freq_present) %>% 
  select(name, 
         number_present, freq_present, 
         number_absent, freq_absent,
         number_first, freq_first,
         number_last, freq_last)


  
# most likely to call on
# most likely to be called on by