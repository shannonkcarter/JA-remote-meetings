library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  todays_order <- reactive({
    df <- data.frame(date = input$date,
                     time = input$time,
                     error = input$errors,
                     Ben = NA,
                     Brian = ifelse("Brian" %in% input$order_order$text, which(input$order_order$text == "Brian"), NA),
                     Carly = ifelse("Carly" %in% input$order_order$text, which(input$order_order$text == "Carly"), NA),
                     David = ifelse("David" %in% input$order_order$text, which(input$order_order$text == "David"), NA),
                     Divine = NA,
                     Emi = ifelse("Emi" %in% input$order_order$text, which(input$order_order$text == "Emi"), NA),
                     Eric = NA,
                     Hala = NA,
                     Jeff = ifelse("Jeff" %in% input$order_order$text, which(input$order_order$text == "Jeff"), NA),
                     Kelsey = ifelse("Kelsey" %in% input$order_order$text, which(input$order_order$text == "Kelsey"), NA),
                     Marissa = NA,
                     Nigel = NA,
                     Shannon = ifelse("Shannon" %in% input$order_order$text, which(input$order_order$text == "Shannon"), NA),
                     Taylor = ifelse("Taylor" %in% input$order_order$text, which(input$order_order$text == "Taylor"), NA),
                     Zach = NA) %>% 
      mutate(error = case_when(error == "Yes :/" ~ "Y",
                               error == "No, flawless execution!" ~ "N"))
    return(df)
  })
  
  output$current_streak_vb <- renderValueBox({
    valueBox(value = current_streak,
             subtitle = tags$p("meetings since last misstep", style = "color: #ffffff !important;"),
             #subtitle = "meetings since last misstep",
             icon = icon("clock"),
             color = "light-blue"
    )
  })
  
  output$longest_streak_vb <- renderValueBox({
    valueBox(value = longest_streak,
             subtitle = tags$p("longest winning streak", style = "color: #ffffff !important;"),
             icon = icon("trophy"),
             color = "light-blue"
             )
  })
  
  colors_data <- reactive({
    df %>% 
      filter(date != "2021-03-11" & date != "2021-03-12") %>% 
      tail(10) %>% 
      select(-c(Ben, Divine, Eric, Hala, Marissa, Nigel, Zach)) %>% 
      mutate(index = 1:length(date)) %>% 
      pivot_longer(Brian:Taylor) %>% 
      mutate(value = factor(value, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), ordered = T))  
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
        select(-c(error, Divine, Hala, Marissa, Zach, Ben, Eric, Nigel))
      return(dt)
    }, options = list(dom = "t", ordering = F, 
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)
    
    output$streak_histogram <- renderPlot({
      ggplot(x, aes(x = numones)) +
        geom_histogram(stat = "count", fill = "#5c9ad2") +
        theme_ja() +
        labs(x = "streak length",
             y = "number of times") +
        scale_x_continuous(breaks = seq(0, 55, 5))
    })
    
    output$streak_pie <- renderPlot({
      pct <- tabyl(misstep_streak, error) %>%
        filter(error == "N") %>%
        pull(percent)

      tabyl(misstep_streak, error) %>%
        ggplot(aes(x="", y=percent, fill=error)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        scale_fill_manual(values = c("#5c9ad2", "white")) +
        theme(legend.position = "none")+
        labs(title = paste0("We get the order correct in", pct, "% of our meetings"))

    })
    
    output$heatmap <- renderPlot({
      who_rates %>% 
        filter(!name %in% c("Hala", "Divine", "Zach", "Marissa", "Eric", "Ben", "Nigel")) %>% 
        filter(!called_on %in% c("Hala", "Divine", "Zach", "Marissa", "Eric", "Ben", "Nigel")) %>% 
        filter(!is.na(total_shared_meetings)) %>% 
        ggplot(aes(x=called_on, y=name, fill=called_on_adj)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#5c9ad2", high = "#FF8B00",
                             midpoint = 0.14, limit = c(0, 0.45)) +
        labs(title = "Who calls on whom?") +
        labs(y = "Person", x = "Calls On", fill = "frequency") +
        theme_ja() +
        theme(legend.position = "bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    })
    
    
    output$three_charts <- renderPlot({
      
      first <- make_bar_chart(freq_first_last, freq_first, name, "% of meetings first", "Who goes first?")
      last <- make_bar_chart(freq_first_last, freq_last, name, "% of meetings last",  "Who goes last?")
      missing <- make_bar_chart(freq_first_last, freq_missing, name, "% of meetings missed",  "Who skips?")
      
      grid.arrange(grobs=list(first, last, missing), ncol=3)
      
    })
    
    output$hist_brian <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Brian") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_carly <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Carly") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_david <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "David") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_emi <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Emi") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    # output$hist_eric <- renderPlot({
    #   hist <- df %>% 
    #     pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
    #     mutate(order = as.numeric(order)) %>% 
    #     filter(person == "Eric") %>%
    #     filter(order < 9) %>% 
    #     ggplot(aes(x = order)) +
    #     geom_bar(stat = "count", fill = "#FF8B00") +
    #     #facet_wrap(~person) +
    #     scale_x_continuous(breaks = seq(1, 8, 1)) + 
    #     theme_void()
    #   hist
    # }, height = 100)
    
    output$hist_jeff <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Jeff") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_kelsey <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Kelsey") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
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
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_taylor <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Taylor") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#FF8B00") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    # output$colors_ben <- renderPlot({
    #   hist <- colors_data() %>% 
    #     make_colors_chart(., "Ben")
    #   hist
    # }, height = 25)
    
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
    
    output$colors_emi <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Emi")
      hist
    }, height = 25)
    
    # output$colors_eric <- renderPlot({
    #   hist <- colors_data() %>% 
    #     make_colors_chart(., "Eric")
    #   hist
    # }, height = 25)
    
    output$colors_jeff <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Jeff")
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
    
    output$colors_taylor <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Taylor")
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
        select(-c(Divine, Hala, Zach, Marissa, Ben, Eric, Nigel))
      return(dt)
    }, options = list(dom = "t", ordering = F, pageLength = 20,
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)

    
    
})

