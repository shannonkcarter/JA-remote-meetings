
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
                              h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Brian"]),  align = "left"),
                              h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Brian"], " (", stats$called_on_x_pct[stats$name == "Brian"], "%)"), align = "left"),
                              h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Brian"], " (", stats$called_on_by_x_pct[stats$name == "Brian"], "%)"), align = "left")
                          )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Carly"),
                              img(src = "carly.jpg", height = 150),
                              tags$hr(),
                              plotOutput("hist_carly", height = "100%"),
                              h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Carly"]),  align = "left"),
                              h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Carly"], " (", stats$called_on_x_pct[stats$name == "Carly"], "%)"), align = "left"),
                              h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Carly"], " (", stats$called_on_by_x_pct[stats$name == "Carly"], "%)"), align = "left")
                              
                      )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("David"),
                          img(src = "david.jpg", height = 150),
                          tags$hr(),
                          plotOutput("hist_david", height = "100%"),
                          h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "David"]),  align = "left"),
                          h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "David"], " (", stats$called_on_x_pct[stats$name == "David"], "%)"), align = "left"),
                          h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "David"], " (", stats$called_on_by_x_pct[stats$name == "David"], "%)"), align = "left")
                   )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Emi"),
                          img(src = "emi.png", height = 150),
                          tags$hr(),
                          plotOutput("hist_emi", height = "100%"),
                          h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Emi"]),  align = "left"),
                          h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Emi"], " (", stats$called_on_x_pct[stats$name == "Emi"], "%)"), align = "left"),
                          h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Emi"], " (", stats$called_on_by_x_pct[stats$name == "Emi"], "%)"), align = "left")
                   ))),
 
    fluidRow(#card_ui
        column(width = 3, offset = 0, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Jeff"),
               img(src = "jeff.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_jeff", height = "100%"),
               h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Jeff"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Jeff"], " (", stats$called_on_x_pct[stats$name == "Jeff"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Jeff"], " (", stats$called_on_by_x_pct[stats$name == "Jeff"], "%)"), align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Kelsey"),
               img(src = "kelsey.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_kelsey", height = "100%"),
               h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Kelsey"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Kelsey"], " (", stats$called_on_x_pct[stats$name == "Kelsey"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Kelsey"], " (", stats$called_on_by_x_pct[stats$name == "Kelsey"], "%)"), align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Marissa"),
               img(src = "marissa.png", height = 150),
               tags$hr(),
               plotOutput("hist_marissa", height = "100%"),
               h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Marissa"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Marissa"], " (", stats$called_on_x_pct[stats$name == "Marissa"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Marissa"], " (", stats$called_on_by_x_pct[stats$name == "Marissa"], "%)"), align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Shannon"),
               img(src = "shannon.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_shannon", height = "100%"),
               h5(paste0("Most likely to go: ", stats$mode_pretty[stats$name == "Shannon"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Shannon"], " (", stats$called_on_x_pct[stats$name == "Shannon"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Shannon"], " (", stats$called_on_by_x_pct[stats$name == "Shannon"], "%)"), align = "left")
        ))) # close fluidRow with 2rd row of stat cards
               ), # close fluidRow with all stat cards
    fluidRow(align = "center",
        # action button reveals scatterplot
        actionButton(inputId = "show_data",
                   label = "Click to show data")
    ),
    tags$hr(),
    conditionalPanel(condition = ("input.show_data%2!=0"),
                     fluidRow(
                         column(width = 12,
                                dataTableOutput('table_past')
                               )
                         ),
                     tags$hr())
    


    ) # close fluidPage
) # close whole app
