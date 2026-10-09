{
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
}

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

# Roster: everyone who has ever been in the data, plus whether they're current.
# To add someone, add a row with active = TRUE. When someone leaves, set active = FALSE.
roster <- read_csv(here::here("roster.csv"), show_col_types = FALSE)
all_members <- roster$name
active_members <- sort(roster$name[roster$active])

# Never drop anyone's history: df is written back to S3 on every submission
not_in_roster <- setdiff(names(df), c("date", "time", "error", all_members))
if (length(not_in_roster) > 0) stop("Add to roster.csv (active = FALSE): ", paste(not_in_roster, collapse = ", "))

# New roster members get an empty column; it's saved to S3 with the next submission
for (p in setdiff(all_members, names(df))) df[[p]] <- NA_character_
df <- select(df, date, time, error, all_of(all_members))

# One row per person per meeting they attended. meeting_id is the row order of df,
# which is more reliable than date (a few rows have mistyped dates)
attended <- df %>% 
  mutate(meeting_id = row_number()) %>% 
  pivot_longer(all_of(all_members), names_to = "name", values_to = "order") %>% 
  filter(!is.na(order)) %>% 
  mutate(order = as.numeric(order))

max_position <- max(attended$order)

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
# Sync fun facts to Google Sheets only when running locally (the deployed app has no Google login)
if (interactive()) {
  tryCatch(
    write_sheet(fun_facts, ss = "https://docs.google.com/spreadsheets/d/1A4lGnWVhg4gsHe6lIUv2_l_4wATiBxt_SlRiOCZHpfE/edit#gid=0", sheet = "all_fun_facts"),
    error = function(e) message("Fun fact sync to Google Sheets failed: ", conditionMessage(e))
  )
}
#write_csv(fun_facts, here::here("fun_facts.csv"))
###--- calculations for data vis and stats
getMode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

modes <- attended %>% 
  group_by(name) %>% 
  summarize(mode = getMode(order)[1]) %>% 
  mutate(mode_pretty = case_when(mode == 1 ~ "1st",
                                 mode == 2 ~ "2nd",
                                 mode == 3 ~ "3rd", 
                                 T ~ paste0(mode, "th")))

#Calculate Number of times each team member was in a meeting with another
shared_meeting_count <- attended %>% 
  select(meeting_id, x1 = name) %>% 
  inner_join(select(attended, meeting_id, x2 = name), by = "meeting_id", relationship = "many-to-many") %>% 
  filter(x1 != x2) %>% 
  count(x1, x2, name = "total_shared_meetings")

#Each pair of consecutive speakers: name called on called_on
call_pairs <- attended %>% 
  arrange(meeting_id, order) %>% 
  group_by(meeting_id) %>% 
  mutate(called_on = lead(name)) %>% 
  ungroup() %>% 
  filter(!is.na(called_on)) %>% 
  select(name, called_on)

#Figure out who called on who and divide by number of times they shared a meeting
who_rates <- call_pairs %>% 
  count(name, called_on) %>% 
  left_join(shared_meeting_count, by = c("name" = "x1", "called_on" = "x2")) %>% 
  mutate(called_on_adj = n/total_shared_meetings)

who_rates_by <- call_pairs %>% 
  select(name = called_on, called_on_by = name) %>% 
  count(name, called_on_by) %>% 
  left_join(shared_meeting_count, by = c("name" = "x1", "called_on_by" = "x2")) %>% 
  mutate(called_on_adj = n/total_shared_meetings)

# Individual stats only name people on the current roster
called_on_most_by <- who_rates_by %>%
  filter(called_on_by %in% active_members) %>%
  group_by(name) %>%
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>%
  arrange(desc(called_on_adj)) %>%
  slice_head(n = 1) %>%
  select(name, called_on_by_most = called_on_by, called_on_by_x_times = n, called_on_by_x_pct = called_on_adj)

stats <- who_rates %>%
  filter(called_on %in% active_members) %>%
  group_by(name) %>%
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>%
  arrange(desc(called_on_adj)) %>%
  slice_head(n = 1) %>%
  select(name, calls_on_most = called_on, called_on_x_times = n, called_on_x_pct = called_on_adj) %>%
  left_join(modes, by = "name") %>%
  left_join(called_on_most_by, by = "name")

#Attendance rate: meetings attended out of all meetings since the person's first one
freq_attended <- attended %>% 
  filter(name %in% active_members) %>% 
  group_by(name) %>% 
  summarize(number_attended = n(),
            number_meetings = nrow(df) - min(meeting_id) + 1) %>% 
  mutate(freq_attended = round((number_attended/number_meetings)*100, 1)) %>% 
  select(name, freq_attended)

freq_first_last <- attended %>% 
  group_by(meeting_id) %>% 
  mutate(last_position = max(order)) %>% 
  ungroup() %>% 
  filter(name %in% active_members) %>% 
  group_by(name) %>% 
  summarize(number_meetings = n(),
            number_last = sum(order == last_position),
            number_first = sum(order == 1)) %>%
  mutate(freq_last = round((number_last/number_meetings)*100, 2),
         freq_first = round((number_first/number_meetings)*100, 2)) %>% 
  left_join(freq_attended, by = "name")

#Personal records: longest run of consecutive meetings attended, and times going first this month
this_month <- floor_date(Sys.Date(), "month")

personal_records <- attended %>%
  filter(name %in% active_members) %>%
  arrange(name, meeting_id) %>%
  group_by(name) %>%
  mutate(run = cumsum(meeting_id != lag(meeting_id, default = -1L) + 1)) %>%
  count(name, run) %>%
  summarize(longest_attendance_streak = max(n)) %>%
  left_join(
    attended %>%
      filter(name %in% active_members, date >= this_month) %>%
      group_by(name) %>%
      summarize(attended_this_month = n(), first_this_month = sum(order == 1)),
    by = "name") %>%
  mutate(across(c(attended_this_month, first_this_month), ~replace_na(.x, 0)))

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


# Share of meetings where we got the lineup right, shown baseball-style (e.g. 0.841)
batting_average <- mean(misstep_streak$error == "N")
