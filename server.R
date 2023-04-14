library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  todays_order <- reactive({
    df <- data.frame(date = input$date,
                     time = "Standup",
                     error = input$errors,
                     Ben = NA,
                     Brian = ifelse("Brian" %in% input$order_order$text, which(input$order_order$text == "Brian"), NA),
                     Carly = ifelse("Carly" %in% input$order_order$text, which(input$order_order$text == "Carly"), NA),
                     David = ifelse("David" %in% input$order_order$text, which(input$order_order$text == "David"), NA),
                     Divia = ifelse("Divia" %in% input$order_order$text, which(input$order_order$text == "Divia"), NA),
                     Divine = NA,
                     Emi = ifelse("Emi" %in% input$order_order$text, which(input$order_order$text == "Emi"), NA),
                     Eric = NA,
                     Gail = NA,
                     Gerard = ifelse("Gerard" %in% input$order_order$text, which(input$order_order$text == "Gerard"), NA),
                     Hala = NA,
                     Jeff = ifelse("Jeff" %in% input$order_order$text, which(input$order_order$text == "Jeff"), NA),
                     Jessica = ifelse("Jessica" %in% input$order_order$text, which(input$order_order$text == "Jessica"), NA),
                     Kelsey = ifelse("Kelsey" %in% input$order_order$text, which(input$order_order$text == "Kelsey"), NA),
                     Malsi = NA,
                     Marissa = NA,
                     Nigel = NA,
                     Shannon = ifelse("Shannon" %in% input$order_order$text, which(input$order_order$text == "Shannon"), NA),
                     Smith = ifelse("Smith" %in% input$order_order$text, which(input$order_order$text == "Smith"), NA),
                     Zach = NA) %>% 
      mutate(error = case_when(error == "Yes :/" ~ "Y",
                               error == "No, flawless execution!" ~ "N"))
    return(df)
  })

  output$current_streak <- renderUI({
    HTML(current_streak)
  })

  output$longest_streak <- renderUI({
    HTML(longest_streak)
  })

  colors_data <- reactive({
    df %>% 
      filter(date != "2021-03-11" & date != "2021-03-12") %>% 
      tail(10) %>% 
      select(-c(Ben, Divine, Eric, Hala, Marissa, Nigel, Zach, Gail)) %>% 
      mutate(index = 1:length(date)) %>% 
      pivot_longer(Brian:Smith) %>% 
      mutate(value = factor(value, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), ordered = T))  
  })
  
  todays_fun_fact <- reactive({
    ff_df <- tibble(date = input$date,
                    time = input$time,
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
    text <- random_fun_fact()$funfact
  })
    
    output$table_today <- renderDataTable({
      dt <- todays_order() %>% 
        select(-c(error, Divine, Hala, Marissa, Zach, Ben, Eric, Nigel, Gail))
      return(dt)
    }, options = list(dom = "t", ordering = F, 
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)
    
    output$streak_histogram <- renderHighchart({
      x %>% count(numones) %>% 
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
                            )}"
                                  )) %>% 
        hc_title(text = "Our streaks tend to be pretty modest", align = "center")
    })
    
    output$streak_pie <- renderHighchart({
      pct <- tabyl(misstep_streak, error) %>%
        filter(error == "N") %>%
        mutate(percent = round(100 * percent, 1)) %>% 
        pull(percent)

      tabyl(misstep_streak, error) %>%
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
    })
    
    output$heatmap <- renderHighchart({
      who_rates %>%
        filter(!name %in% c("Hala", "Divine", "Zach", "Marissa", "Eric", "Ben", "Nigel", "Gail")) %>%
        filter(!called_on %in% c("Hala", "Divine", "Zach", "Marissa", "Eric", "Ben", "Nigel", "Gail")) %>%
        filter(!is.na(total_shared_meetings)) %>%
        mutate(called_on_adj = round(100 * called_on_adj, 1)) %>%
        select("from" = name, "to" = called_on, weight = called_on_adj) %>%
        hchart("dependencywheel") %>%
        hc_title(text = "Who calls on whom?", align = "center") %>%
        hc_add_theme(ja_hc_theme()) %>%
        hc_colors(c(ja_hex("red"), ja_hex("orange"), ja_hex("yellow"), "#9acd32",  ja_hex("green"),
                    ja_hex("blue"), "#00008B", "#7f00ff", ja_hex("purple3"), ja_hex("purple")))
    })
    
    
    output$three_charts <- renderUI({
      b <- freq_first_last %>% 
        select(name, "goes first" = freq_first, "goes last" = freq_last, "skips" = freq_missing) %>% 
        pivot_longer(cols = "goes first":"skips", names_to = "var", values_to = "value")
      
      
      purrr::map(unique(b$var), function(x) {
        b %>% 
          filter(var == x) %>% 
          arrange(desc(value)) %>% 
          hchart("bar", hcaes(y = value, x = name)) %>% 
          hc_title(text = paste0("Who ", x, "?")) %>%
          hc_add_theme(ja_hc_theme()) %>% 
          hc_colors("#5c9ad2") %>% 
          hc_yAxis(title = list(text = "% of meetings"),
                   min = 0, max = 30) %>% 
          hc_xAxis(title = list(text = "")) %>% 
          hc_tooltip(formatter = JS("function(){
                                return (
                              this.point.name + ' ' + this.point.var +
                              ' in ' + this.point.value + '% of meetings'
                                )}"))
      }) %>% 
        hw_grid(rowheight = 350, ncol = 3) 
      
    })
    
    output$hist_brian <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Brian") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_carly <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Carly") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_david <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "David") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_divia <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Divia") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_emi <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Emi") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_gerard <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Gerard") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_jeff <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Jeff") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_jessica <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Jessica") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_kelsey <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Kelsey") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    # output$hist_nigel <- renderPlot({
    #   hist <- df %>% 
    #     pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
    #     mutate(order = as.numeric(order)) %>% 
    #     filter(person == "Nigel") %>%
    #     filter(order < 9) %>% 
    #     ggplot(aes(x = order)) +
    #     geom_bar(stat = "count", fill = "#FF8B00") +
    #     #facet_wrap(~person) +
    #     scale_x_continuous(breaks = seq(1, 8, 1), limits = c(1,8)) + 
    #     xlim(1,8) +
    #     theme_void()
    #   hist
    # }, height = 100)

    
    output$hist_shannon <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Shannon") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$hist_smith <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Smith") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#319CF4") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 60)
    
    output$colors_brian <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Brian")
      hist
    }, height = 25)
    
    output$colors_carly <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Carly")
      hist
    }, height = 25)
    
    output$colors_david <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "David")
      hist
    }, height = 25)
    
    output$colors_divia <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Divia")
      hist
    }, height = 25)
    
    output$colors_emi <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Emi")
      hist
    }, height = 25)
    
    output$colors_gerard <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Gerard")
      hist
    }, height = 25)
    
    output$colors_jeff <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Jeff")
      hist
    }, height = 25)
    
    output$colors_jessica <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Jessica")
      hist
    }, height = 25)
    
    output$colors_kelsey <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Kelsey")
      hist
    }, height = 25)
    
    # output$colors_nigel <- renderPlot({
    #   hist <- colors_data() %>%
    #     make_colors_chart(., "Nigel")
    #   hist
    # }, height = 25)
    
    output$colors_shannon <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Shannon")
      hist
    }, height = 25)
    
    output$colors_smith <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Smith")
      hist
    }, height = 25)
    
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
        select(-c(Divine, Hala, Zach, Marissa, Ben, Eric, Nigel, Gail))
      return(dt)
    }, options = list(dom = "t", ordering = F, pageLength = 20,
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)

    
    
})

