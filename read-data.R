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

# initially - push data to aws bucket
df <- read_csv(here::here("standup_data.csv")) %>%
  mutate(date = as.Date(date_y, format = "%m/%d/%y")) %>%
  select(date, time, Brian, Carly, David, Divine, Emi, Hala, Jeff, Kelsey, Marissa, Shannon, Zach)
s3saveRDS(df, bucket = "standupapp", object = "standapp-data.rds")

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



