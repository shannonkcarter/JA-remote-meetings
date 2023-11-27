library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(aws.s3)

app_password <- config::get("submit", file = "config.yml")$app_pw

pw <- config::get("aws", file = "config.yml")

Sys.setenv("AWS_ACCESS_KEY_ID" = pw$AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" =  pw$AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = pw$AWS_DEFAULT_REGION
)

loadData <- function() {
  df <- s3readRDS(
    bucket = "standupapp",
    object = "standapp-data.rds"
  )
  return(df)
}

df <- loadData() 


wrapped <- df %>% 
  select(date, time, error, 
         Brian, Carly, David, Divia, Gerard, Jeff, Jessica, Kelsey, Smith, Shannon) %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2023) %>% 
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

###--- WHO CALLS ON WHO -------------
columns <- colnames(wrapped)
columns
names <- columns[!columns %in% c("date", "time", "error")]
names
all_combos <- combn(names, 2) %>% 
  as.data.frame() %>% 
  mutate(
    rowid = row_number()
  ) %>% 
  dplyr::select(rowid, everything()) %>% 
  pivot_longer(V1:last_col(), names_to = "name1", values_to = "name2") %>% 
  pivot_wider(names_from = "rowid" , values_from = "name2") %>% 
  dplyr::select(-name1) %>% 
  clean_names() %>% 
  mutate(row_id = row_number())

shared_meetings <- wrapped %>% 
  select(-error) %>% 
  mutate(
    Brian = ifelse(!is.na(Brian), "Brian", NA),
    Carly = ifelse(!is.na(Carly), "Carly", NA),
    David = ifelse(!is.na(David), "David", NA),
    Divia = ifelse(!is.na(Divia), "Divia", NA),
    Gerard = ifelse(!is.na(Gerard), "Gerard", NA),
    Jeff = ifelse(!is.na(Jeff), "Jeff", NA),
    Jessica = ifelse(!is.na(Jessica), "Jessica", NA),
    Kelsey = ifelse(!is.na(Kelsey), "Kelsey", NA),
    Smith = ifelse(!is.na(Smith), "Smith", NA),
    Shannon = ifelse(!is.na(Shannon), "Shannon", NA)
  ) 

count_shared_meetings <- function(i) {
  select_combo <- filter(all_combos, row_id==i)
  
  count_shared_meetings <- shared_meetings %>% 
    dplyr::select(x1 = select_combo[[1]], x2 = select_combo[[2]]) %>% 
    filter(!is.na(x1) & !is.na(x2)) %>% 
    count(x1, x2)
  return(count_shared_meetings)
}

shared_meeting_count <- all_combos %>%
  group_by(row_id) %>% 
  do(count_shared_meetings(.$row_id)) %>% 
  ungroup() %>% 
  dplyr::select(-row_id) %>% 
  distinct()

who_called_on_who <- wrapped %>% 
  select(-error) %>% 
  pivot_longer(`Brian`:`Shannon`, names_to = "name", values_to = "order") %>% 
  mutate(time = factor(time, levels = c("Standup", "Sitdown"))) %>% 
  filter(!is.na(order)) %>% 
  arrange(date, time, order) %>% 
  group_by(date, time) %>% 
  mutate(
    total_n = max(order, na.rm=T),
    called_on = lead(name)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(called_on)) %>% 
  count(name, called_on)

#Figure out who called on who and divide by number of times they shared a meeting
who_called_on_by <- wrapped %>% 
  select(-error) %>% 
  pivot_longer(`Brian`:`Shannon`, names_to = "name", values_to = "order") %>% 
  mutate(time = factor(time, levels = c("Standup", "Sitdown"))) %>% 
  filter(!is.na(order)) %>% 
  arrange(date, time, order) %>% 
  group_by(date, time) %>% 
  mutate(
    total_n = max(order, na.rm=T),
    called_on_by = lag(name)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(called_on_by)) %>% 
  count(name, called_on_by)

who_rates_to <- who_called_on_who %>% 
  left_join(., shared_meeting_count, by=c("name" = "x1", "called_on" = "x2")) %>% 
  left_join(., shared_meeting_count, by=c("name" = "x2", "called_on" = "x1")) %>% 
  mutate(
    total_shared_meetings = ifelse(is.na(n.y), n, n.y),
    called_on_adj = round(100*n.x/total_shared_meetings, 1)
  ) %>% 
  dplyr::select(name, called_on, n = n.x, total_shared_meetings, called_on_adj) %>% 
  group_by(name) %>% 
  slice_max(called_on_adj) %>% 
  ungroup() %>% 
  select(name, person_calls_on_most = called_on, freq_calls_on_most = called_on_adj) #%>% 
  # mutate(person_called_by_most = c(),
  #        freq_called_by_most =)

who_rates_from <- who_called_on_who %>% 
  left_join(., shared_meeting_count, by=c("name" = "x1", "called_on" = "x2")) %>% 
  left_join(., shared_meeting_count, by=c("name" = "x2", "called_on" = "x1")) %>% 
  mutate(
    total_shared_meetings = ifelse(is.na(n.y), n, n.y),
    called_on_adj = round(100*n.x/total_shared_meetings, 1)
  ) %>% 
  dplyr::select(name, called_on, n = n.x, total_shared_meetings, called_on_adj) %>% 
  group_by(called_on) %>% 
  slice_max(called_on_adj) %>% 
  select(name = called_on, person_called_by_most = name, freq_called_by_most = called_on_adj) 

export <- left_join(freq_first_last, who_rates_to) %>% 
  left_join(who_rates_from) %>% 
  write_csv("wrapped_stats.csv")
