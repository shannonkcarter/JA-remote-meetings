
source("read-data.R")
source("helper-functions.R")

# Define UI for application that draws a histogram
shinyUI(div(class="page-wrapper",
    tags$head(tags$title("JA Stand-app!"),
              tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
    tags$link(rel="preconnect", href="https://fonts.googleapis.com", crossorigin="true"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Onest:wght@400;500;600;700&display=swap"),
    shinyjs::useShinyjs(),
    useShinydashboardPlus(),

    # Application title
    div(class="title",
        img(src = "ja-logo.svg", alt = "January Advisors"),
        img(src = "standapp.svg", alt = "Stand-app!")
        ),

    # Sidebar with a slider input for number of bins
    #sidebarLayout(

    # Main input section
    div(
        class="main-input-wrapper",
        div(class="main-input-section",
            div(
                class="form-fields",

                div(class="form-row",
                    # radioButtons(
                    #     inputId = "time",
                    #     label = "Standup or Sitdown?",
                    #     choices = c("Standup", "Sitdown")
                    # ),
                    dateInput(
                      inputId = "date",
                      format = "M d, yyyy",
                      label = "Enter today's date:",
                      width = "30%"
                    ),
                    radioButtons(
                        inputId = "errors", label = "Any errors?",
                        choices = c("Yes :/", "No, flawless execution!"),
                        selected = "No, flawless execution!"
                    )
                ),
                textInput(
                    "fun_fact", label = "Fun Fact:", placeholder = "Enter today's fun fact!",
                    width = "calc(100% - 40px)"
                ),
                div(class="form-row",
                    radioButtons(
                        inputId = "fun_fact_fun", label = "Was the fun fact fun?", 
                        choices = c("Yes!", "Not really :(")
                    ),
                    radioButtons(
                        inputId = "fun_fact_fact", label = "Was the fun fact factual?",
                        choices = c("Yes!", "Not really :/")
                    )
                )
            ),
            div(
                class="order-fields",
                orderInput(
                    inputId = 'people',
                    label = 'Roster: ',
                    items = active_members,
                    connect = c('order', "missing"),
                    item_class = "info",
                    width = "100%"
                ),
                orderInput(
                    inputId = 'order',
                    label = 'Drag names here to set the lineup:',
                    items = NULL,
                    connect = c('people', 'missing'),
                    placeholder = ''
                ),
                orderInput(
                    inputId = 'missing',
                    label = "On the bench: ",
                    items = NULL,
                    connect = c("people", "order"),
                    placeholder = ""
                )
            )
        ),

        # Submit data section!
        fluidRow(align = "center",
            actionButton(
                inputId = "submit",
                label ="Submit Data!",
                #class = "btn-primary",
                icon = icon("baseball")
            )
        ),
        shinyjs::hidden(
            div(
                id = "thankyou_msg",
                h3("Thanks, your response was submitted successfully!")
            )
        ),  

    ),

    div(
        class="streaks",
        div(
            class="streak-counters",
            div(
                h2('Current Hitting Streak'),
                uiOutput("current_streak")
            ),
            div(
                h2('Longest Hitting Streak'),
                uiOutput("longest_streak")
            ),
            div(
                h2('Batting Average'),
                uiOutput("batting_average"),
                #p(class="streak-caption", "how often we get the lineup right")
            )
        ),
        div(
            class="streak-graphs",
            highchartOutput("streak_histogram")
        )
    ),

    tags$hr(),

    fluidRow(
        column(4, offset=1,
            highchartOutput("heatmap",  height = "350px")
        ),

        column(6,
               htmlOutput("three_charts", height = "350px")

        ),

    ),

    br(),
    div(align = "center",
             actionButton("show_funfact",
                          "Show me a fun fact!",
                          icon = icon("lightbulb"))),
    br(),
    div(align = "center", shinyjs::hidden(
        div(
            id = "funfact_random",
            h4(htmlOutput("funfact"))
        )
    )),

    div(
        class="individual-stats",
        h2("Baseball Cards"),
        do.call(div, c(list(class="personal-stats-wrapper"), lapply(active_members, person_panel)))
    ),

    fluidRow(align = "center",
        class="individual-stats",
        # action button reveals scatterplot
        actionButton(inputId = "show_data",
                   label = "Click to show data",
                   icon = icon("table"))
    ),
    conditionalPanel(condition = ("input.show_data%2!=0"),
                     fluidRow(
                         class="individual-stats",
                                dataTableOutput('table_past')
                         ),
                     tags$hr())
    


    )
) # close whole app

