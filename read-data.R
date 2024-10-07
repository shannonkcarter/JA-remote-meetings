library(shiny)
library(shinyjqui)
library(DT)
library(tidyverse)
library(here)
library(config)
library(aws.s3)
library(shinyjs)
library(jastyle)
library(lubridate)
library(janitor)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(gridExtra)
library(plotly)
library(highcharter)
library(googlesheets4)


# # initially - push data to aws bucket
# df <- read_csv(here::here("standup_data.csv")) %>%
#   mutate(date = as.Date(date_y, format = "%m/%d/%y")) %>%
#   select(date, time, Brian, Carly, David, Divine, Emi, Hala, Jeff, Kelsey, Marissa, Shannon, Zach)

# s3saveRDS(fun_facts, bucket = "standupapp", object = "funfact-data.rds")
# s3saveRDS(df, bucket = "standupapp", object = "standapp-data.rds")

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

# # need to do if adding another person...
# todays_order <- c(date = "2024-09-23", time = "Sitdown", error = "N",
#                   Ben = NA, Brian = NA, Anna = NA, Carly = NA, David = NA, Divia = NA, Divine = NA, Emi = NA,
#                   Eric = NA, Gail = NA, Gerard = NA, Hala = NA, Jeff = NA, Jessica = NA, Kelsey = NA,
#                   Katie = NA, Kevin = NA, Malsi = NA, Marissa = NA,
#                   Nigel = NA, Sarah = 1, Shannon = NA, Smith = NA, Zach = NA)
# df <- df %>%
#   mutate("Sarah" = NA) %>%
#   select(date, time, error, Ben, Brian, Anna, Carly, David, Divia, Divine, Emi,
#          Eric, Gail, Gerard, Hala, Jeff, Jessica, Kelsey, Katie, Kevin, Malsi, Marissa, Nigel,
#          Sarah, Shannon, Smith, Zach) %>%
#   rbind(todays_order)
# 
# s3saveRDS(df,
#           bucket = "standupapp",
#           object = "standapp-data.rds")

loadData_ff <- function() {
  df <- s3readRDS(
    bucket = "standupapp",
    object = "funfact-data.rds"
  ) %>% 
    filter(funfact != "") %>% 
    distinct()
  return(df)
}

fun_facts <- loadData_ff() 
#write_sheet(fun_facts, ss = "https://docs.google.com/spreadsheets/d/1A4lGnWVhg4gsHe6lIUv2_l_4wATiBxt_SlRiOCZHpfE/edit#gid=0", sheet = "all_fun_facts")
#write_csv(fun_facts, here::here("fun_facts.csv"))
###--- calculations for data vis and stats
getMode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

modes <- df %>% 
  summarize(Ben = getMode(Ben)[1],
            Brian = getMode(Brian)[1],
            Anna = getMode(Anna)[1],
            Carly = getMode(Carly)[1],
            David = getMode(David)[1],
            Divia = getMode(Divia)[1],
            Divine = getMode(Divine)[1],
            Emi = getMode(Emi)[1],
            Eric = getMode(Eric)[1],
            Gail = getMode(Gail)[1],
            Gerard = getMode(Gerard)[1],
            Hala = getMode(Hala)[1],
            Jeff = getMode(Jeff)[1],
            Jessica = getMode(Jessica)[1],
            Katie = getMode(Katie)[1],
            Kelsey = getMode(Kelsey)[1],
            Kevin = getMode(Kevin)[1],
            Marissa = getMode(Marissa)[1],
            #Masi = getMode(Malsi)[1],
            Nigel = getMode(Nigel)[1],
            Sarah = getMode(Sarah)[1],
            Shannon = getMode(Shannon)[1],
            Smith = getMode(Smith)[1],
            Zach = getMode(Zach)[1]) %>% 
  pivot_longer(cols = Ben:Zach, names_to = "name", values_to = "mode") %>% 
  mutate(mode_pretty = case_when(mode == 1 ~ "1st",
                                 mode == 2 ~ "2nd",
                                 mode == 3 ~ "3rd", 
                                 T ~ paste0(mode, "th")))

#Calculate Number of times each team member was in a meeting with another
columns <- colnames(df)
columns
names <- columns[!columns %in% c("date", "time", "error")]
names
all_combos <- combn(names, 2) %>% 
  as.data.frame() %>% 
  mutate(
    rowid = row_number()
  ) %>% 
  dplyr::select(rowid, everything()) %>% 
  # need to change to # of columns when adding someone new
  pivot_longer(V1:last_col(), names_to = "name1", values_to = "name2") %>% 
  pivot_wider(names_from = "rowid" , values_from = "name2") %>% 
  dplyr::select(-name1) %>% 
  clean_names() %>% 
  mutate(row_id = row_number())

shared_meetings <- df %>% 
  select(-error) %>% 
  mutate(
    Ben = ifelse(!is.na(Ben), "Ben", NA),
    Brian = ifelse(!is.na(Brian), "Brian", NA),
    Anna = ifelse(!is.na(Anna), "Anna", NA),
    Carly = ifelse(!is.na(Carly), "Carly", NA),
    David = ifelse(!is.na(David), "David", NA),
    Divia = ifelse(!is.na(Divia), "Divia", NA),
    Divine = ifelse(!is.na(Divine), "Divine", NA),
    Emi = ifelse(!is.na(Emi), "Emi", NA),
    Eric = ifelse(!is.na(Eric), "Eric", NA),
    Gail = ifelse(!is.na(Gail), "Gail", NA),
    Gerard = ifelse(!is.na(Gerard), "Gerard", NA),
    Hala = ifelse(!is.na(Hala), "Hala", NA),
    Jeff = ifelse(!is.na(Jeff), "Jeff", NA),
    Jessica = ifelse(!is.na(Jessica), "Jessica", NA),
    Kelsey = ifelse(!is.na(Kelsey), "Kelsey", NA),
    Katie = ifelse(!is.na(Katie), "Katie", NA),
    Kevin = ifelse(!is.na(Kevin), "Kevin", NA),
    Marissa = ifelse(!is.na(Marissa), "Marissa", NA),
    Nigel = ifelse(!is.na(Nigel), "Nigel", NA),
    Sarah = ifelse(!is.na(Sarah), "Sarah", NA),
    Shannon = ifelse(!is.na(Shannon), "Shannon", NA),
    Smith = ifelse(!is.na(Smith), "Smith", NA),
    Zach = ifelse(!is.na(Zach), "Zach", NA),
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
  dplyr::select(-row_id)

#Figure out who called on who and divide by number of times they shared a meeting
who_called_on_who <- df %>% 
  select(-error) %>% 
  pivot_longer(`Ben`:`Zach`, names_to = "name", values_to = "order") %>% 
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
who_called_on_by <- df %>% 
  select(-error) %>% 
  pivot_longer(`Ben`:`Zach`, names_to = "name", values_to = "order") %>% 
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


who_rates <- who_called_on_who %>%
  left_join(., shared_meeting_count, by=c("name" = "x1", "called_on" = "x2")) %>%
  left_join(., shared_meeting_count, by=c("name" = "x2", "called_on" = "x1")) %>%
  mutate(
    total_shared_meetings = ifelse(is.na(n.y), n, n.y),
    called_on_adj = n.x/total_shared_meetings
  ) %>%
  dplyr::select(name, called_on, n = n.x, total_shared_meetings, called_on_adj)

who_rates_by <- who_called_on_by %>%
  left_join(., shared_meeting_count, by=c("name" = "x1", "called_on_by" = "x2")) %>%
  left_join(., shared_meeting_count, by=c("name" = "x2", "called_on_by" = "x1")) %>%
  mutate(
    total_shared_meetings = ifelse(is.na(n.y), n, n.y),
    called_on_adj = n.x/total_shared_meetings
  ) %>%
  dplyr::select(name, called_on_by, n = n.x, total_shared_meetings, called_on_adj)

called_on_most_by <- who_rates_by %>%
  #filter(n > 5) %>%
  group_by(name) %>%
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>%
  arrange(desc(called_on_adj)) %>%
  slice_head(n = 1) %>%
  select(name, called_on_by_most = called_on_by, called_on_by_x_times = n, called_on_by_x_pct = called_on_adj)

stats <- who_rates %>%
  #filter(n > 5) %>%
  group_by(name) %>%
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>%
  arrange(desc(called_on_adj)) %>%
  slice_head(n = 1) %>%
  select(name, calls_on_most = called_on, called_on_x_times = n, called_on_x_pct = called_on_adj) %>%
  left_join(modes) %>%
  left_join(called_on_most_by)


meetings_since <- as.numeric(length(df$date[as.numeric(rownames(df)) > 432]))
meetings_since_interns <- as.numeric(length(df$date[as.numeric(rownames(df)) > 523]))
meetings_since_nigel <- as.numeric(length(df$date[as.numeric(rownames(df)) > 566]))
meetings_since_smith <- as.numeric(length(df$date[as.numeric(rownames(df)) > 789]))
meetings_since_gerard <- as.numeric(length(df$date[as.numeric(rownames(df)) > 866]))
meetings_since_gail <- as.numeric(length(df$date[as.numeric(rownames(df)) > 961]))
meetings_since_malsi = as.numeric(length(df$date[as.numeric(rownames(df)) > 1044]))
meetings_since_divia = as.numeric(length(df$date[as.numeric(rownames(df)) > 1263]))
meetings_since_jessica = as.numeric(length(df$date[as.numeric(rownames(df)) > 1311]))
meetings_since_kevin = as.numeric(length(df$date[as.numeric(rownames(df)) > 1340]))
meetings_since_katie = as.numeric(length(df$date[as.numeric(rownames(df)) > 1460]))
meetings_since_anna = as.numeric(length(df$date[as.numeric(rownames(df)) > 1533]))
meetings_since_sarah = as.numeric(length(df$date[as.numeric(rownames(df)) > 1584]))

freq_missing <- df %>% 
  ungroup() %>% 
  select(-c("Anna", "Ben", "Divine", "Eric", "Hala", "Gail", "Malsi",  "Marissa", "Nigel", "Zach", "Smith", 
            "Sarah", "Jessica")) %>% 
  pivot_longer(`Brian`:`Shannon`, names_to = "name", values_to = "order") %>% 
  mutate(time = factor(time, levels = c("Standup", "Sitdown"))) %>% 
  filter(!is.na(order)) %>%
  mutate(order = as.numeric(order)) %>% 
  group_by(name) %>% 
  reframe(number_attended = length(name),
            number_meetings = case_when(#name == "Ben" ~ 0 + meetings_since_interns,
                                        name == "Brian" ~ 432 + meetings_since,
                                        name == "Anna" ~ meetings_since,
                                        name == "Carly" ~ 324 + meetings_since_anna,
                                        name == "David" ~ 432 + meetings_since,
                                        name == "Emi" ~ 188 + meetings_since,
                                        #name == "Gail" ~ meetings_since_gail,
                                        name == "Gerard" ~ meetings_since_gerard,
                                        #name == "Eric" ~ 0 +meetings_since_interns,
                                        name == "Jeff" ~ 432 + meetings_since,
                                        name == "Jessica" ~ meetings_since_jessica,
                                        name == "Kelsey" ~ 432 + meetings_since,
                                        name == "Kevin" ~ meetings_since_kevin,
                                        #name == "Malsi" ~ meetings_since_malsi,
                                        #name == "Marissa" ~ 173 + meetings_since,
                                        #name == "Nigel" ~ 0 + meetings_since_nigel,
                                        name == "Sarah" ~ meetings_since_sarah,
                                        name == "Shannon" ~ 432 + meetings_since,
                                        #name == "Smith" ~ meetings_since_smith,
                                        name == "Divia" ~ meetings_since_divia,
                                        name == "Katie" ~ meetings_since_katie)) %>%
  mutate(freq_missing = round(100 - (number_attended/number_meetings)*100, 1)) %>% 
  distinct() %>% 
  select(name, freq_missing)

freq_first_last <- df %>% 
  select(-c("Anna", "Ben", "Divine", "Eric", "Gail", "Hala", "Malsi", "Marissa", "Nigel", "Zach", "Emi", "Kevin", "Smith", 
            "Sarah", "Jessica")) %>% 
  pivot_longer(`Brian`:`Shannon`, names_to = "name", values_to = "order") %>% 
  mutate(time = factor(time, levels = c("Standup", "Sitdown"))) %>% 
  filter(!is.na(order)) %>% 
  group_by(date, time) %>% 
  mutate(last_position = max(order)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarize(number_meetings = length(order),
            number_last = length(order[order == last_position]),
            number_first = length(order[order == 1])) %>%
  mutate(freq_last = round((number_last/number_meetings)*100, 2),
         freq_first = round((number_first/number_meetings)*100, 2)) %>% 
  left_join(freq_missing)

misstep_streak <- df %>%
  dplyr::select(date, time, error) %>% 
  filter(!is.na(error)) %>%
  mutate(errorless_streak = case_when(error == "N" ~ 1,
                                      error == "Y" ~ 0)) 

zis <- c(which(misstep_streak[,'errorless_streak']==0), length(misstep_streak$error))
x <- data.frame(x=seq_along(zis),numones=diff(c(0L,zis))-1L)
longest_streak <- max(x$numones) + 1
current_streak <- tail(x, 1) %>% 
  # who knows why but i have to do this - saves correctly (eg for longest streak) after its over
  mutate(current_streak = numones + 1) %>% 
  pull(current_streak)

# csa = df %>% 
#   select(date, time, Carly, Shannon) %>% 
#   filter(!is.na(Carly)) %>% 
#   filter(!is.na(Shannon)) %>% 
#   filter(date > "2020-07-01") %>% 
#   mutate(Carly = as.numeric(Carly),
#          Shannon = as.numeric(Shannon)) %>% 
#   mutate(diff = abs(Carly - Shannon))
