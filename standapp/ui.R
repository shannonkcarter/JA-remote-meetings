library(shiny)
library(shinyjqui)
library(DT)
library(tidyverse)
library(googlesheets4)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("January Advisors Remote Meetings"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                       dateInput(inputId = "date",
                                 format = "M d, yyyy",
                                 label = "Enter today's date")),
                column(6,
                       radioButtons(inputId = "time",
                                    label = "",
                                    choices = c("Standup", "Sitdown"))
                       )),
            orderInput(inputId = 'people',
                       label = 'Our Team: ', 
                       items = c('Brian', 'Carly', 'David', "Emi", "Jeff", "Kelsey", "Marissa", "Shannon"), 
                       connect = c('order', "missing"),
                       item_class = "info",
                       width = "100%"),
            orderInput(inputId = 'order', 
                       label = 'Standup Order: ', 
                       items = NULL, 
                       connect = c('people', 'missing'),
                       placeholder = 'Drag people here to designate order...'),
            orderInput(inputId = 'missing',
                       label = "Truants: ", 
                       items = NULL, 
                       connect = c("people", "order"),
                       placeholder = "Drag people missing the meeting here"),
            actionButton(inputId = "submit",
                         label = "Submit Data!",
                         icon = icon("chevron-right"))
            
            ), # close sidebarPanel
        
        mainPanel(
            fluidRow(style="padding:20px 0 20px 0;",
                     column(12, align="center",
                            dataTableOutput('todays_order')
                     )
            ),
            fluidRow(plotOutput("hists"))
            ) # close main panel
        ) # close sidebarLayout
    ) # close fluidPage
) # close whole app
