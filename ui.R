
source("read-data.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # blue #5c9ad2
    # orange #f59035
    tags$head(tags$script(src = "message-handler.js")),
    shinyjs::useShinyjs(),
    tags$style(HTML("
    @font-face {
        
        font-family: 'Roboto';
        }
                    .well {
        background-color: #5c9ad2 !important; 
        
        margin-bottom: 10px !important;
                    }
        .btn-info {
        background-color: #f59035 !important;
        border-color: #FFFFFF;
        
        border-radius: 12px;
        }
        body {
        color: #FFFFFF;
        }

                    "
                    )),

    # Application title
    column(12, align = "center",
           img(src = "ja_logo.png", height = 100)),
           #h1("January Advisors StandApp")),
    
    tags$hr(),

    # Sidebar with a slider input for number of bins
    #sidebarLayout(
    column(10, offset = 1,    
    wellPanel(
            fluidRow(
                column(3, 
                       dateInput(inputId = "date",
                                 format = "M d, yyyy",
                                 label = "Enter today's date:"),
                       radioButtons(inputId = "time",
                                    label = "",
                                    choices = c("Standup", "Sitdown"))),
            column(6, offset = 1,
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
                              placeholder = 'Drag names here to designate order...'),
                   orderInput(inputId = 'missing',
                              label = "Truants: ",
                              items = NULL,
                              connect = c("people", "order"),
                              placeholder = "Drag people missing the meeting here")),
            column(1,
                   actionButton(inputId = "submit",
                                label ="Submit Data!",
                                #class = "btn-primary",
                                icon = icon("chevron-right")))
            ),
            shinyjs::hidden(
                div(
                    id = "thankyou_msg",
                    h3("Thanks, your response was submitted successfully!")
                )
            )  
            ) # close wellPanel
    ), # close column
        
            fluidRow(style="padding:20px 0 20px 0;",
                     column(10, offset = 1, align="center",
                            dataTableOutput('table_today')
                     )
            ),

    
    ## individual stat cards
    
    fluidRow(
        #shiny::HTML("<center><h3>Stats</h3></center>"),
        
               fluidRow(
                   column(width = 3, offset = 0, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Brian"),
                              img(src = "brian.jpg", height = 150),
                              tags$hr(),
                              plotOutput("hist_brian", height = "100%"),
                              h5("Most likely to go: 1st", align = "left"),
                              h5("Most likely to call on: Jeff", align = "left"),
                              h5("Most likely to be called on by: Kelsey", align = "left")
                          )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Carly"),
                              img(src = "carly.jpg", height = 150),
                              tags$hr(),
                              plotOutput("hist_carly", height = "100%"),
                              h5("Most likely to go: 1st", align = "left"),
                              h5("Most likely to call on: Jeff", align = "left"),
                              h5("Most likely to be called on by: Kelsey", align = "left")
                              
                      )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("David"),
                          img(src = "david.jpg", height = 150),
                          tags$hr(),
                          plotOutput("hist_david", height = "100%"),
                          h5("Most likely to go: 1st", align = "left"),
                          h5("Most likely to call on: Jeff", align = "left"),
                          h5("Most likely to be called on by: Kelsey", align = "left")
                   )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Emi"),
                          img(src = "emi.png", height = 150),
                          tags$hr(),
                          plotOutput("hist_emi", height = "100%"),
                          h5("Most likely to go: 1st", align = "left"),
                          h5("Most likely to call on: Jeff", align = "left"),
                          h5("Most likely to be called on by: Kelsey", align = "left")
                   ))),
 
    fluidRow(#card_ui
        column(width = 3, offset = 0, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Jeff"),
               img(src = "jeff.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_jeff", height = "100%"),
               h5("Most likely to go: 1st", align = "left"),
               h5("Most likely to call on: Jeff", align = "left"),
               h5("Most likely to be called on by: Kelsey", align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Kelsey"),
               img(src = "kelsey.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_kelsey", height = "100%"),
               h5("Most likely to go: 1st", align = "left"),
               h5("Most likely to call on: Jeff", align = "left"),
               h5("Most likely to be called on by: Kelsey", align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Marissa"),
               img(src = "marissa.png", height = 150),
               tags$hr(),
               plotOutput("hist_marissa", height = "100%"),
               h5("Most likely to go: 1st", align = "left"),
               h5("Most likely to call on: Jeff", align = "left"),
               h5("Most likely to be called on by: Kelsey", align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Shannon"),
               img(src = "shannon.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_shannon", height = "100%"),
               h5("Most likely to go: 1st", align = "left"),
               h5("Most likely to call on: Jeff", align = "left"),
               h5("Most likely to be called on by: Kelsey", align = "left")
        )))
               )


    ) # close fluidPage
) # close whole app
