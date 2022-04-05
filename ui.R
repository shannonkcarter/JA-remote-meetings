
source("read-data.R")
source("helper-functions.R")

# Define UI for application that draws a histogram
shinyUI(div(class="page-wrapper",
    # blue #5c9ad2
    # orange #FF8B00
    tags$head(tags$script(src = "message-handler.js")),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
    tags$link(rel="preconnect", href="https://fonts.googleapis.com", crossorigin="true"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Lobster&display=swap"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Roboto+Condensed:ital,wght@0,300;0,400;0,700;1,400&display=swap"),
    shinyjs::useShinyjs(),
    useShinydashboardPlus(),

    # Application title
    # column(12, align = "center",
    #        img(src = "ja_logo.png", height = 100)),
    #        #h1("January Advisors StandApp")),
    div(class="title", 
        img(src = "ja_logo.png"),
        img(src = "standapp.svg"),
    ),

    # Sidebar with a slider input for number of bins
    #sidebarLayout(

    # Main input section
    div(
        class="main-input-wrapper",
        div(class="main-input-section",
            div(
                class="form-fields",
                dateInput(
                    inputId = "date",
                    format = "M d, yyyy",
                    label = "Enter today's date:"
                ),
                div(class="form-row",
                    radioButtons(
                        inputId = "time",
                        label = "Standup or Sitdown?",
                        choices = c("Standup", "Sitdown")
                    ),
                    radioButtons(
                        inputId = "errors", label = "Did we misstep?",
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
                    label = 'Meeting Order: ',
                    items = c('Brian', 'Carly', 'David', "Emi", "Gerard", "Jeff", "Kelsey", "Shannon", "Taylor"),
                    connect = c('order', "missing"),
                    item_class = "info",
                    width = "100%"
                ),
                orderInput(
                    inputId = 'order',
                    label = 'Drag names here to designate order:',
                    items = NULL,
                    connect = c('people', 'missing'),
                    placeholder = ''
                ),
                orderInput(
                    inputId = 'missing',
                    label = "Truants: ",
                    items = NULL,
                    connect = c("people", "order"),
                    placeholder = ""
                )
            )
        ),
        # # Today's pre-submit table
        # fluidRow(
        #     column(10, offset = 1, align="center",
        #         dataTableOutput('table_today')
        #     )
        # ),

        # Submit data section!
        fluidRow(align = "center",
            actionButton(
                inputId = "submit",
                label ="Submit Data!",
                #class = "btn-primary",
                icon = icon("chevron-right"),
                style="color: #fff; background-color: #FF8B00; border-color: #fff"
            )
        ),
        shinyjs::hidden(
            div(
                id = "thankyou_msg",
                h3("Thanks, your response was submitted successfully!")
            )
        ),  

    ),

    #
    div(
        class="streaks",
        div(
            class="streak-counters",
            div(
                h2('Current Streak'),
                uiOutput("current_streak")
            ),
            div(
                h2('Longest Streak'),
                uiOutput("longest_streak")
            )
        ),
        div(
            class="streak-graphs",
            highchartOutput("streak_pie"),
            highchartOutput("streak_histogram"),
        )
    ),
    # fluidRow(column(12, h2("Streaks: "))),
    # fluidRow(
    #     column(3, highchartOutput("streak_pie")),
    #     column(4, highchartOutput("streak_histogram")),
    #     column(4, 
    #         br(),
    #         br(),
    #         br(),
    #         fluidRow(valueBoxOutput("current_streak_vb", width = 9)),
    #         br(),
    #         fluidRow(valueBoxOutput("longest_streak_vb", width = 9))
    #     )
    # ),
    
    tags$hr(),

    # Team stats
    # div(
    #     class="team-stats",
    #     div(
    #         highchartOutput("heatmap",  height = "350px")
    #     ),
    #     div(
    #         htmlOutput("three_charts", height = "350px")
    #     ),
    # ),
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
                          icon = icon("lightbulb"),
                          style="color: #fff; background-color: #FF8B00; border-color: #fff")),
    br(),
    div(align = "center", shinyjs::hidden(
        div(
            id = "funfact_random",
            h4(htmlOutput("funfact"))
        )
    )),
    
    div(
        class="individual-stats",
        h2("Individual Stats: "),
        div(
            class="personal-stats-wrapper",
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Brian")
                    ),
                    img(src = "brian.jpg", class="profile-img", height = 150),
                    plotOutput("hist_brian", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Brian"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Brian"], " (", stats$called_on_x_pct[stats$name == "Brian"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Brian"], " (", stats$called_on_by_x_pct[stats$name == "Brian"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_brian", height = "100%")
                    )
                )
            ),
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Carly")
                    ),
                    img(src = "carly.jpg", class="profile-img", height = 150),
                    plotOutput("hist_carly", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Carly"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Carly"], " (", stats$called_on_x_pct[stats$name == "Carly"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Carly"], " (", stats$called_on_by_x_pct[stats$name == "Carly"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_carly", height = "100%")
                    )
                )
            ),
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("David")
                    ),
                    img(src = "david.jpg", class="profile-img", height = 150),
                    plotOutput("hist_david", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "David"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "David"], " (", stats$called_on_x_pct[stats$name == "David"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "David"], " (", stats$called_on_by_x_pct[stats$name == "David"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_david", height = "100%")
                    )
                )
            ),
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Emi")
                    ),
                    img(src = "emi.jpg", class="profile-img", height = 150),
                    plotOutput("hist_emi", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Emi"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Emi"], " (", stats$called_on_x_pct[stats$name == "Emi"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Emi"], " (", stats$called_on_by_x_pct[stats$name == "Emi"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_emi", height = "100%")
                    )
                )
            ),
            
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Gerard")
                    ),
                    img(src = "gerard.jpg", class="profile-img", height = 150),
                    plotOutput("hist_gerard", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Gerard"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Gerard"], " (", stats$called_on_x_pct[stats$name == "Gerard"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Gerard"], " (", stats$called_on_by_x_pct[stats$name == "Gerard"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_gerard", height = "100%")
                    )
                )
            ),
            
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Jeff")
                    ),
                    img(src = "jeff.jpg", class="profile-img", height = 150),
                    plotOutput("hist_jeff", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Jeff"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Jeff"], " (", stats$called_on_x_pct[stats$name == "Jeff"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Jeff"], " (", stats$called_on_by_x_pct[stats$name == "Jeff"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_jeff", height = "100%")
                    )
                )
            ),
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Kelsey")
                    ),
                    img(src = "kelsey.jpg", class="profile-img", height = 150),
                    plotOutput("hist_kelsey", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Kelsey"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Kelsey"], " (", stats$called_on_x_pct[stats$name == "Kelsey"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Kelsey"], " (", stats$called_on_by_x_pct[stats$name == "Kelsey"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_kelsey", height = "100%")
                    )
                )
            ),
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Shannon")
                    ),
                    img(src = "shannon.jpg", class="profile-img", height = 150),
                    plotOutput("hist_shannon", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Shannon"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Shannon"], " (", stats$called_on_x_pct[stats$name == "Shannon"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Shannon"], " (", stats$called_on_by_x_pct[stats$name == "Shannon"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_shannon", height = "100%")
                    )
                )
            ),
            div(
                wellPanel(
                    div(
                        class="personal-stats-header",
                        h3("Taylor")
                    ),
                    img(src = "taylor.jpg", class="profile-img", height = 150),
                    plotOutput("hist_taylor", height = "100%", width= '70%'),
                    div(class="indv-written-stats",
                        h5(paste0("Most often goes: ", stats$mode_pretty[stats$name == "Taylor"]),  align = "left"),
                        h5(paste0("Most likely to call on: ", stats$calls_on_most[stats$name == "Taylor"], " (", stats$called_on_x_pct[stats$name == "Taylor"], "%)"), align = "left"),
                        h5(paste0("Most likely to be called on by: ", stats$called_on_by_most[stats$name == "Taylor"], " (", stats$called_on_by_x_pct[stats$name == "Taylor"], "%)"), align = "left"),
                    ),
                    div(class="recent-activity",
                        h5("Recent activity"),
                        plotOutput("colors_taylor", height = "100%")
                    )
                )
            )
        )
    ),

    fluidRow(align = "center",
        class="individual-stats",
        # action button reveals scatterplot
        actionButton(inputId = "show_data",
                   label = "Click to show data",
                   icon = icon("table"),
                   style="color: #fff; background-color: #FF8B00; border-color: #fff")
    ),
    conditionalPanel(condition = ("input.show_data%2!=0"),
                     fluidRow(
                         class="individual-stats",
                                dataTableOutput('table_past')
                         ),
                     tags$hr())
    


    )
) # close whole app

