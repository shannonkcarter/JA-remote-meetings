
source("read-data.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # blue #5c9ad2
    # orange #f59035
    tags$head(tags$script(src = "message-handler.js")),
    shinyjs::useShinyjs(),
   
    tags$style(HTML("
    body {
        
        font-family: 'Roboto' !important;
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
        h2 {
        color: #000000 !important;
        }
        h4 {
        color: #000000 !important;
        }
        p {
        color: #000000 !important;
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
    column(12, offset = 0,    
    wellPanel(
            fluidRow(
                column(2, 
                       dateInput(inputId = "date",
                                 format = "M d, yyyy",
                                 label = "Enter today's date:"),
                       radioButtons(inputId = "time",
                                    label = "",
                                    choices = c("Standup", "Sitdown"))),
            column(6, offset = 1,
                   orderInput(inputId = 'people',
                              label = 'Meeting Order: ',
                              items = c('Brian', 'Carly', 'David', "Emi", "Jeff", "Kelsey", "Marissa", "Shannon"),
                              connect = c('order', "missing"),
                              item_class = "info",
                              width = "100%"),
                   orderInput(inputId = 'order',
                              label = 'Drag names here to designate order:',
                              items = NULL,
                              connect = c('people', 'missing'),
                              placeholder = ''),
                   orderInput(inputId = 'missing',
                              label = "Truants: ",
                              items = NULL,
                              connect = c("people", "order"),
                              placeholder = "")),
            column(3,

                   textInput("fun_fact", label = "Fun Fact:", placeholder = "Enter today's fun fact!",
                             width = "100%"),
                   fluidRow(
                       column(6,
                   radioButtons(inputId = "fun_fact_fun", label = "Was the fun fact fun?", 
                                choices = c("Yes!", "Not really :("))),
                   column(6,
                   radioButtons(inputId = "fun_fact_fact", label = "Was the fun fact factual?",
                                choices = c("Yes!", "Not really :/")))))
            ),
            shinyjs::hidden(
                div(
                    id = "thankyou_msg",
                    h3("Thanks, your response was submitted successfully!")
                )
            )  
            ) # close wellPanel
    ), # close column
    fluidRow(align = "center",
        actionButton(inputId = "submit",
                 label ="Submit Data!",
                 #class = "btn-primary",
                 icon = icon("chevron-right"),
                 style="color: #fff; background-color: #f59035; border-color: #fff")),
        
            fluidRow(style="padding:20px 0 20px 0;",
                     column(10, offset = 1, align="center",
                            dataTableOutput('table_today')
                     )
            ),
    
    fluidRow(column(6, offset = 1, 
                    h2("Team Stats: "))),
    fluidRow(column(4, offset = 1, align = "center",
                    h4("Who calls on whom?"),
                    plotOutput("heatmap", width = "500px")),
             column(2, offset = 1, align = "center",
                    h4("Who goes first?"),
                    img(src = "carly.jpg", height = 150),
                    p("Most likely"),
                    br(),
                    img(src = "shannon.jpg", height = 150),
                    p("Least likely")),
             column(2, offset = 1, align = "center",
                    h4("Who goes last?"),
                    img(src = "jeff.jpg", height = 150),
                    p("Most likely"),
                    br(),
                    img(src = "marissa.png", height = 150),
                    p("Least likely")
                    )),

    tags$hr(),
    ## individual stat cards
    fluidRow(column(6, offset = 1, 
                    h2("Individual Stats: "))),
    
    fluidRow(
        #shiny::HTML("<center><h3>Stats</h3></center>"),
        column(width = 10, offset = 1,       
        fluidRow(
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Brian"),
                              img(src = "brian.jpg", height = 150),
                              # flipBox(id = 'brian_flip',
                              #         trigger = "hover",
                              #         front = div(img(src = "brian.jpg", height = 150)),
                              #         back = div(img(src = "brian_today.png", height = 150))
                              #     ),
                              tags$hr(),
                              plotOutput("hist_brian", height = "100%"),
                              h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Brian"]),  align = "left"),
                              h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Brian"], " (", stats$called_on_x_pct[stats$name == "Brian"], "%)"), align = "left"),
                              h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Brian"], " (", stats$called_on_by_x_pct[stats$name == "Brian"], "%)"), align = "left")
                          )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Carly"),
                              img(src = "carly.jpg", height = 150),
                              tags$hr(),
                              plotOutput("hist_carly", height = "100%"),
                              h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Carly"]),  align = "left"),
                              h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Carly"], " (", stats$called_on_x_pct[stats$name == "Carly"], "%)"), align = "left"),
                              h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Carly"], " (", stats$called_on_by_x_pct[stats$name == "Carly"], "%)"), align = "left")
                              
                      )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("David"),
                          img(src = "david.jpg", height = 150),
                          tags$hr(),
                          plotOutput("hist_david", height = "100%"),
                          h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "David"]),  align = "left"),
                          h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "David"], " (", stats$called_on_x_pct[stats$name == "David"], "%)"), align = "left"),
                          h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "David"], " (", stats$called_on_by_x_pct[stats$name == "David"], "%)"), align = "left")
                   )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                              h3("Emi"),
                          img(src = "emi.png", height = 150),
                          tags$hr(),
                          plotOutput("hist_emi", height = "100%"),
                          h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Emi"]),  align = "left"),
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
               h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Jeff"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Jeff"], " (", stats$called_on_x_pct[stats$name == "Jeff"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Jeff"], " (", stats$called_on_by_x_pct[stats$name == "Jeff"], "%)"), align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Kelsey"),
               img(src = "kelsey.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_kelsey", height = "100%"),
               h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Kelsey"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Kelsey"], " (", stats$called_on_x_pct[stats$name == "Kelsey"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Kelsey"], " (", stats$called_on_by_x_pct[stats$name == "Kelsey"], "%)"), align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Marissa"),
               img(src = "marissa.png", height = 150),
               tags$hr(),
               plotOutput("hist_marissa", height = "100%"),
               h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Marissa"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Marissa"], " (", stats$called_on_x_pct[stats$name == "Marissa"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Marissa"], " (", stats$called_on_by_x_pct[stats$name == "Marissa"], "%)"), align = "left")
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
                   h3("Shannon"),
               img(src = "shannon.jpg", height = 150),
               tags$hr(),
               plotOutput("hist_shannon", height = "100%"),
               h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Shannon"]),  align = "left"),
               h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Shannon"], " (", stats$called_on_x_pct[stats$name == "Shannon"], "%)"), align = "left"),
               h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Shannon"], " (", stats$called_on_by_x_pct[stats$name == "Shannon"], "%)"), align = "left")
        ))) # close fluidRow with 2rd row of stat cards
               )), # close fluidRow with all stat cards
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

