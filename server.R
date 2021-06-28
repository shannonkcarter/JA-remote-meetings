library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  todays_order <- reactive({
    df <- data.frame(date = input$date,
                     time = input$time,
                     error = input$errors,
                     Ben = ifelse("Ben" %in% input$order_order$text, which(input$order_order$text == "Ben"), NA),
                     Brian = ifelse("Brian" %in% input$order_order$text, which(input$order_order$text == "Brian"), NA),
                     Carly = ifelse("Carly" %in% input$order_order$text, which(input$order_order$text == "Carly"), NA),
                     David = ifelse("David" %in% input$order_order$text, which(input$order_order$text == "David"), NA),
                     Divine = NA,
                     Emi = ifelse("Emi" %in% input$order_order$text, which(input$order_order$text == "Emi"), NA),
                     Eric = ifelse("Eric" %in% input$order_order$text, which(input$order_order$text == "Eric"), NA),
                     Hala = NA,
                     Jeff = ifelse("Jeff" %in% input$order_order$text, which(input$order_order$text == "Jeff"), NA),
                     Kelsey = ifelse("Kelsey" %in% input$order_order$text, which(input$order_order$text == "Kelsey"), NA),
                     Marissa = NA,
                     Nigel = ifelse("Nigel" %in% input$order_order$text, which(input$order_order$text == "Nigel"), NA),
                     Shannon = ifelse("Shannon" %in% input$order_order$text, which(input$order_order$text == "Shannon"), NA),
                     Zach = NA) %>% 
      mutate(error = case_when(error == "Yes :/" ~ "Y",
                               error == "No, flawless execution!" ~ "N"))
    return(df)
  })
  
  colors_data <- reactive({
    df %>% 
      filter(date != "2021-03-11" & date != "2021-03-12") %>% 
      tail(10) %>% 
      select(-c(Divine, Hala, Marissa, Zach)) %>% 
      mutate(index = 1:length(date)) %>% 
      pivot_longer(Ben:Shannon) %>% 
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
        select(-c(error, Divine, Hala, Marissa, Zach))
      return(dt)
    }, options = list(dom = "t", ordering = F, 
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)
    
    output$heatmap <- renderPlot({
      who_rates %>% 
        filter(name != "Hala" & name != "Divine" & name != "Zach" & name != "Marissa") %>% 
        filter(called_on != "Hala" & called_on != "Divine" & called_on != "Zach" & called_on != "Marissa") %>% 
        filter(!is.na(total_shared_meetings)) %>% 
        # mutate(text = paste0(name, " calls on ", called_on, " in ", round(called_on_adj*100), 
        #                      "% of meetings they attend together")) %>% 
        
        ggplot(aes(x=called_on, y=name, fill=called_on_adj)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#5c9ad2", high = "#f59035",
                             midpoint = 0.14, limit = c(0, 0.45)) +
        labs(title = "Who calls on whom?") +
        theme_bw() +
        labs(y = "Person", x = "Calls On", fill = "frequency") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              text = element_text(size = 14, family = "Roboto"),
              legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title.position = "plot",
              plot.title = element_text(face="bold")
              )
      # ggplotly(chart,
      #          tooltip = chart$text)
    })
    
    
    output$three_charts <- renderPlot({
      
      first <- make_bar_chart(freq_first_last, freq_first, name, "% of meetings first", "Who goes first?")
      last <- make_bar_chart(freq_first_last, freq_last, name, "% of meetings last",  "Who goes last?")
      missing <- make_bar_chart(freq_first_last, freq_missing, name, "% of meetings missed",  "Who skips?")
      
      grid.arrange(grobs=list(first, last, missing), ncol=3)
      
    })
    
    output$hist_ben <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Ben") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
        scale_x_continuous(breaks = seq(1, 8, 1), limits = c(1,8)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_brian <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Brian") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
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
        geom_bar(stat = "count", fill = "#f59035") +
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
        geom_bar(stat = "count", fill = "#f59035") +
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
        geom_bar(stat = "count", fill = "#f59035") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_eric <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Eric") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_jeff <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Jeff") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
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
        geom_bar(stat = "count", fill = "#f59035") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_nigel <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Nigel") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1), limits = c(1,8)) + 
        theme_void()
      hist
    }, height = 100)

    
    output$hist_shannon <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Ben:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Shannon") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$colors_ben <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Ben")
      hist
    }, height = 25)
    
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
    
    output$colors_eric <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Eric")
      hist
    }, height = 25)
    
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
    
    output$colors_nigel <- renderPlot({
      hist <- colors_data() %>%
        make_colors_chart(., "Nigel")
      hist
    }, height = 25)
    
    output$colors_shannon <- renderPlot({
      hist <- colors_data() %>% 
        make_colors_chart(., "Shannon")
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
        select(-c(Divine, Hala, Zach, Marissa))
      return(dt)
    }, options = list(dom = "t", ordering = F, pageLength = 20,
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)

    
    
})

