library(tidyverse)
library(aws.s3)
library(highcharter)
library(RCurl)
library(htmlwidgets)

source("read-data.R")


upload_to_ja_site <- function(.chart, .fn) {
  fn <- .fn
  htmlwidgets::saveWidget(.chart, here::here("figures",fn), selfcontained = TRUE)
  user <- "maps@januaryadvisors.com"
  pwd <- "RFubsvfhb4NjYpGs"
  RCurl::ftpUpload(what = here::here("figures",fn), 
                   to = paste("ftp://januaryadvisors.com:21/",fn,sep=""), userpwd = paste(user, pwd, sep = ":"))
}


dependence_wheel = who_rates %>%
  filter(!name %in% c("Hala", "Divine", "Zach", "Marissa", "Eric", "Ben", "Nigel")) %>%
  filter(!called_on %in% c("Hala", "Divine", "Zach", "Marissa", "Eric", "Ben", "Nigel")) %>%
  filter(!is.na(total_shared_meetings)) %>% 
  mutate(called_on_adj = round(100 * called_on_adj, 1)) %>% 
  select("from" = name, "to" = called_on, weight = called_on_adj) %>% 
  hchart("dependencywheel") %>% 
  hc_title(text = "Who calls on whom?", align = "center") %>% 
  hc_add_theme(ja_hc_theme()) %>% 
  hc_colors(c(ja_hex("red"), ja_hex("orange"), ja_hex("yellow"), "#9acd32",  ja_hex("green"),
              ja_hex("blue"), "#00008B", "#7f00ff", ja_hex("purple")))
upload_to_ja_site(dependence_wheel, "ja-remote-chart1.html")


pct <- tabyl(misstep_streak, error) %>%
  filter(error == "N") %>%
  mutate(percent = round(100 * percent, 1)) %>% 
  pull(percent)

pie <- tabyl(misstep_streak, error) %>%
  mutate(error = case_when(error == "N" ~ "right",
                           error == "Y" ~ "wrong"),
         percent = round(100 * percent, 1)) %>% 
  hchart("pie", hcaes(error, percent)) %>% 
  hc_plotOptions(series = list(showInLegend = F, dataLabels = F)) %>% 
  hc_add_theme(ja_hc_theme()) %>% 
  hc_colors(c("#5c9ad2","#FF8B00")) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (
                                  'We get the order ' + this.point.error + ' ' +
                                  this.point.percent + '% of the time'
                                  )}")) %>% 
  hc_title(text = paste0("We get the order correct in ", pct, "% of our meetings"), align = "center")


hist = x %>% count(numones) %>% 
  hchart("column", hcaes(x = numones, y = n)) %>% 
  hc_colors("#5c9ad2") %>% 
  hc_add_theme(ja_hc_theme()) %>% 
  hc_xAxis(title = list(text = "streak length")) %>% 
  hc_yAxis(title = list(text = "number of times")) %>% 
  hc_tooltip(formatter = JS("function(){
                                return (
                                'We have had a streak of ' +
                                this.point.numones + ' meetings ' +
                                this.point.n + ' times.'
                            )}")) %>% 
  hc_title(text = "Our streaks tend to be pretty modest", align = "center")

upload_to_ja_site(pie, "ja-remote-chart2.html")
upload_to_ja_site(hist, "ja-remote-chart3.html")

