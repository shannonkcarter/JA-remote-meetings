
source("read-data.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # blue #5c9ad2
    # orange #f59035
    tags$head(tags$script(src = "message-handler.js")),
    shinyjs::useShinyjs(),
    tags$style(HTML("
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
                            dataTableOutput('recent_data')
                     )
            ),

    
    ## individual stat cards
    
    fluidRow(
        #shiny::HTML("<center><h3>Stats</h3></center>"),
        
               fluidRow(
                   column(width = 3, offset = 0, align = "center",style="padding:20px;",
                          wellPanel(
                               img(src = "brian.jpg", height = 150),
                               actionLink(inputId = "stats_brian", 
                                          label = h5("Brian")),
                               plotOutput("hist_brian")
                          )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                      img(src = "carly.jpg", height = 150),
                      actionLink(inputId = "stats_carly", 
                                 label = h5("Carly"))
                      )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                          img(src = "david.jpg", height = 150),
                          actionLink(inputId = "stats_david", 
                                     label = h5("David"))
                   )),
                   column(width = 3, align = "center",style="padding:20px;",
                          wellPanel(
                          img(src = "emi.png", height = 150),
                          actionLink(inputId = "stats_emi", 
                                     label = h5("Emi"))
                   ))),
 
    fluidRow(
        column(width = 3, offset = 0, align = "center",style="padding:20px;",
               wellPanel(
               img(src = "jeff.jpg", height = 150),
               actionLink(inputId = "stats_jeff", 
                          label = h5("Jeff"))
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
               img(src = "kelsey.jpg", height = 150),
               actionLink(inputId = "stats_kelsey", 
                          label = h5("Kelsey"))
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
               img(src = "marissa.png", height = 150),
               actionLink(inputId = "stats_marissa", 
                          label = h5("Marissa"))
        )),
        column(width = 3, align = "center",style="padding:20px;",
               wellPanel(
               img(src = "shannon.jpg", height = 150),
               actionLink(inputId = "stats_shannon", 
                          label = h5("Shannon"))
        )))
               ),
    
    fluidRow(
        column(8, offset = 2, align = "center", 
               plotOutput("hists"))),


    ) # close fluidPage
) # close whole app
