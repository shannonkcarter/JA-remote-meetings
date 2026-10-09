library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observe(print(todays_order()))
  todays_order <- reactive({
    # Each person's position in today's order (NA if they weren't in it), one column per roster member
    positions <- as.list(setNames(as.character(match(all_members, input$order_order$text)), all_members))

    df <- tibble(date = input$date,
                 time = "Standup",
                 error = case_when(input$errors == "Yes :/" ~ "Y",
                                   input$errors == "No, flawless execution!" ~ "N"),
                 !!!positions)
    return(df)
  })

  output$current_streak <- renderUI({
    HTML(current_streak)
  })

  output$longest_streak <- renderUI({
    HTML(longest_streak)
  })

  # Last 10 meetings, for the "Last 10 games" strips
  colors_data <- df %>%
    tail(10) %>%
    mutate(index = 1:length(date)) %>%
    pivot_longer(all_of(active_members)) %>%
    mutate(value = factor(value, levels = as.character(seq_len(max_position)), ordered = T))
  
  todays_fun_fact <- reactive({
    ff_df <- tibble(date = input$date,
                    time = "Standup",
                    funfact = input$fun_fact,
                    fun = input$fun_fact_fun,
                    fact = input$fun_fact_fact)
    return(ff_df)
  })
  
  
  random_fun_fact <- eventReactive(input$show_funfact, {
    fact <- fun_facts[sample(nrow(fun_facts), 1), ]
    return(fact)
  })
  
  output$funfact <- renderUI({
    req(random_fun_fact())
    fact <- random_fun_fact()
    tagList(
      h4(fact$funfact),
      if (!is.na(fact$date)) p(class = "fun-fact-date", format(fact$date, "%B %e, %Y"))
    )
  })

  output$on_this_day <- renderUI({
    fact <- on_this_day_fact(fun_facts)
    if (is.null(fact)) return(p(class = "fun-fact-date", "No fun facts from around this date in past years."))
    tagList(
      h4(fact$funfact),
      p(class = "fun-fact-date", format(fact$date, "%B %e, %Y"))
    )
  })
    
    output$table_today <- renderDataTable({
      dt <- todays_order() %>% 
        select(date, time, all_of(active_members))
      return(dt)
    }, options = list(dom = "t", ordering = F, 
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)
    
    output$streak_histogram <- renderHighchart({
      x %>% count(numones) %>% 
        hchart("column", hcaes(x = numones, y = n)) %>% 
        hc_chart(backgroundColor = "transparent") %>% 
        hc_colors("#004E7B") %>% 
        hc_add_theme(ja_hc_theme()) %>% 
        hc_xAxis(title = list(text = "hitting streak length"),
                 plotLines = list(list(
                   value = median(x$numones),
                   color = 'black',
                   width = 1,
                   zIndex = 4,
                   dashStyle = "dash",
                   label = list(text = paste0("median: ", median(x$numones)),
                                style = list( color = 'black')
                   )))) %>% 
        hc_yAxis(title = list(text = "number of times")) %>% 
        hc_tooltip(formatter = JS("function(){
                                return (
                                'We have had a hitting streak of ' +
                                this.point.numones + ' games ' +
                                this.point.n + ' times.'
                            )}"
                                  )) %>% 
        hc_title(text = "Our hitting streaks tend to be pretty modest", align = "center")
    })
    
    output$batting_average <- renderUI({
      HTML(sprintf("%.3f", batting_average))
    })
    
    output$heatmap <- renderHighchart({
      wheel_data <- who_rates %>%
        filter(name %in% active_members, called_on %in% active_members) %>%
        filter(!is.na(total_shared_meetings)) %>%
        mutate(called_on_adj = round(100 * called_on_adj, 1)) %>%
        select("from" = name, "to" = called_on, weight = called_on_adj)

      # Give each person their own color (only people with data, or the wheel shows empty nodes)
      wheel_people <- intersect(active_members, c(wheel_data$from, wheel_data$to))
      nodes <- lapply(wheel_people, function(p) list(id = p, color = member_colors[[p]]))

      highchart() %>%
        hc_add_series(type = "dependencywheel", name = "Calls on", data = list_parse(wheel_data), nodes = nodes) %>%
        hc_title(text = "Who calls on whom?", align = "center") %>%
        hc_add_theme(ja_hc_theme())
    })
    
    
    output$three_charts <- renderUI({
      # First and last share an axis so they're comparable; attendance is out of 100
      first_last_max <- ceiling(max(freq_first_last$freq_first, freq_first_last$freq_last) / 10) * 10
      charts <- list(
        list(col = "freq_first",    title = "Who bats leadoff?",           var = "bats leadoff in", max = first_last_max),
        list(col = "freq_last",     title = "Who bats cleanup?",           var = "bats cleanup in",  max = first_last_max),
        list(col = "freq_attended", title = "Who suits up?",              var = "suits up for",   max = 100)
      )

      purrr::map(charts, function(chart) {
        freq_first_last %>%
          mutate(value = .data[[chart$col]], var = chart$var) %>%
          arrange(desc(value)) %>%
          hchart("bar", hcaes(y = value, x = name)) %>%
          hc_title(text = chart$title, style = list(fontSize = "16px")) %>%
          hc_add_theme(ja_hc_theme()) %>%
          hc_colors("#004E7B") %>%
          hc_yAxis(title = list(text = "% of games"),
                   min = 0, max = chart$max) %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_tooltip(formatter = JS("function(){
                                return (
                              this.point.name + ' ' + this.point.var +
                              ' ' + this.point.value + '% of games'
                                )}"))
      }) %>%
        hw_grid(rowheight = 350, ncol = 3) 
      
    })
    
    # Individual stats charts for each active member (see person_panel() in helper-functions.R)
    lapply(active_members, function(person) {
      output[[paste0("hist_", tolower(person))]] <- renderPlot({
        make_order_hist(attended, person)
      }, height = 60)
      
      output[[paste0("colors_", tolower(person))]] <- renderPlot({
        make_colors_chart(colors_data, person)
      }, height = 34)
    })
    
    #Submit password
    observeEvent(input$submit, {
      showModal(
        modalDialog(
          title = h2("Enter password to submit data"),
          textInput(inputId = "password_input", ""),
          actionButton("submit_pw", "Submit password"),
          easyClose = T
        )
      )
    })
    
    #Submit data
    observeEvent(input$submit_pw, {
      #password protection
      if (input$password_input == app_password) {
        #Save data
        s3saveRDS(rbind(df, todays_order()),
                  bucket = "standupapp",
                  object = "standapp-data.rds")
        # save fun fact
        s3saveRDS(rbind(fun_facts, todays_fun_fact()),
                  bucket = "standupapp",
                  object = "funfact-data.rds")
        
        removeModal()
        #shinyjs::reset("form")
        #shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
        
      } 
    })
    
    observeEvent(input$show_funfact, {
 
      shinyjs::show("funfact_random")
    })
    
    output$table_past <- renderDataTable({
      dt <- df %>% 
        tail(20) %>% 
        select(date, time, error, all_of(active_members))
      return(dt)
    }, options = list(dom = "t", ordering = F, pageLength = 20,
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)

    
    
})

