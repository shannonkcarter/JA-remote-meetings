library(shiny)
library(shinyjqui)
library(DT)
library(tidyverse)
library(googlesheets4)
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

# # initially - push data to aws bucket
# df <- read_csv(here::here("standup_data.csv")) %>%
#   mutate(date = as.Date(date_y, format = "%m/%d/%y")) %>%
#   select(date, time, Brian, Carly, David, Divine, Emi, Hala, Jeff, Kelsey, Marissa, Shannon, Zach)
# funfacts <- tibble(date = as.Date("3/17/21", format = "%m/%d/%y"),
#                        time = "Standup",
#                        funfact = "testing",
#                        fun = "N",
#                        fact = "N")
# s3saveRDS(funfacts, bucket = "standupapp", object = "funfact-data.rds")
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

loadData_ff <- function() {
  df <- s3readRDS(
    bucket = "standupapp",
    object = "funfact-data.rds"
  )
  return(df)
}

fun_facts <- loadData_ff()

###--- calculations for data vis and stats
#Start with wide standapp dataset

getMode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

modes <- df %>% 
  summarize(Brian = getMode(Brian)[1],
            Carly = getMode(Carly)[1],
            David = getMode(David)[1],
            Divine = getMode(Divine)[1],
            Emi = getMode(Emi)[1],
            Hala = getMode(Hala)[1],
            Jeff = getMode(Jeff)[1],
            Kelsey = getMode(Kelsey)[1],
            Marissa = getMode(Marissa)[1],
            Shannon = getMode(Shannon)[1],
            Zach = getMode(Zach)[1]) %>% 
  pivot_longer(cols = Brian:Zach, names_to = "name", values_to = "mode") %>% 
  mutate(mode_pretty = case_when(mode == 1 ~ "1st",
                                 mode == 2 ~ "2nd",
                                 mode == 3 ~ "3rd", 
                                 T ~ paste0(mode, "th")))

#Calculate Number of times each team member was in a meeting with another
columns <- colnames(df)
names <- columns[!columns %in% c("date", "time")]

all_combos <- combn(names, 2) %>% 
  as.data.frame() %>% 
  mutate(
    rowid = row_number()
  ) %>% 
  dplyr::select(rowid, everything()) %>% 
  pivot_longer(V1:V55, names_to = "name1", values_to = "name2") %>% 
  pivot_wider(names_from = "rowid" , values_from = "name2") %>% 
  dplyr::select(-name1) %>% 
  clean_names() %>% 
  mutate(row_id = row_number())

shared_meetings <- df %>% 
  mutate(
    Brian = ifelse(!is.na(Brian), "Brian", NA),
    Carly = ifelse(!is.na(Carly), "Carly", NA),
    David = ifelse(!is.na(David), "David", NA),
    Divine = ifelse(!is.na(Divine), "Divine", NA),
    Emi = ifelse(!is.na(Emi), "Emi", NA),
    Hala = ifelse(!is.na(Hala), "Hala", NA),
    Jeff = ifelse(!is.na(Jeff), "Jeff", NA),
    Kelsey = ifelse(!is.na(Kelsey), "Kelsey", NA),
    Marissa = ifelse(!is.na(Marissa), "Marissa", NA),
    Shannon = ifelse(!is.na(Shannon), "Shannon", NA),
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
  pivot_longer(`Brian`:`Zach`, names_to = "name", values_to = "order") %>% 
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
  pivot_longer(`Brian`:`Zach`, names_to = "name", values_to = "order") %>% 
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
  filter(n > 5) %>% 
  group_by(name) %>% 
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>% 
  arrange(desc(called_on_adj)) %>% 
  slice_head(n = 1) %>% 
  select(name, called_on_by_most = called_on_by, called_on_by_x_times = n, called_on_by_x_pct = called_on_adj)

# heatmap <- who_rates %>% 
#   filter(name != "Hala" & name != "Divine" & name != "Zach") %>% 
#   filter(called_on != "Hala" & called_on != "Divine" & called_on != "Zach") %>% 
#   filter(!is.na(total_shared_meetings)) %>% 
#   ggplot(aes(x=called_on, y=name, fill=called_on_adj)) + 
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "#000080", high = "#f59035",
#                        midpoint = 0.14, limit = c(0, 0.4)) +
#   theme_bw() +
#   labs(y = "Person", x = "Calls On", fill = "frequency") +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         text = element_text(size = 14, family = "Roboto"))
# heatmap

stats <- who_rates %>% 
  filter(n > 5) %>% 
  group_by(name) %>% 
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>% 
  arrange(desc(called_on_adj)) %>% 
  slice_head(n = 1) %>% 
  select(name, calls_on_most = called_on, called_on_x_times = n, called_on_x_pct = called_on_adj) %>% 
  left_join(modes) %>% 
  left_join(called_on_most_by)


meetings_since <- length(df$date[as.numeric(rownames(df)) > 432])
freq_missing <- df %>% 
  select(-c("Divine", "Hala", "Zach")) %>% 
  pivot_longer(`Brian`:`Shannon`, names_to = "name", values_to = "order") %>% 
  mutate(time = factor(time, levels = c("Standup", "Sitdown"))) %>% 
  filter(!is.na(order)) %>% 
  group_by(name) %>% 
  summarize(number_attended = length(name),
            number_meetings = case_when(name == "Brian" ~ 432 + meetings_since,
                                        name == "Carly" ~ 324 + meetings_since,
                                        name == "David" ~ 432 + meetings_since,
                                        name == "Emi" ~ 188 + meetings_since,
                                        name == "Jeff" ~ 432 + meetings_since,
                                        name == "Kelsey" ~ 432 + meetings_since,
                                        name == "Marissa" ~ 173 + meetings_since,
                                        name == "Shannon" ~ 432 + meetings_since)) %>%
  mutate(freq_missing = round(100 - (number_attended/number_meetings)*100, 1)) %>% 
  distinct() %>% 
  select(name, freq_missing)

freq_first_last <- df %>% 
  select(-c("Divine", "Hala", "Zach")) %>% 
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

# ggplot(freq_first_last, aes(x = freq_first, y = reorder(name, freq_first))) +
#   geom_bar(stat = "identity") + 
#   labs(x = "Percentage of meetings going first",
#        y = "person") + 
#   theme_bw()
# ggplot(freq_first_last, aes(x = freq_last, y = reorder(name, freq_last))) +
#   geom_bar(stat = "identity") + 
#   labs(x = "Percentage of meetings going last",
#        y = "person") + 
#   theme_bw()
# ggplot(freq_first_last, aes(x = freq_missing, y = reorder(name, freq_missing))) +
#   geom_bar(stat = "identity") + 
#   labs(x = "Percentage of meetings missed",
#        y = "person") + 
#   theme_bw()
