library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  todays_order <- reactive({
    df <- data.frame(date = input$date,
                     time = input$time,
                     Brian = ifelse("Brian" %in% input$order_order$text, which(input$order_order$text == "Brian"), NA),
                     Carly = ifelse("Carly" %in% input$order_order$text, which(input$order_order$text == "Carly"), NA),
                     David = ifelse("David" %in% input$order_order$text, which(input$order_order$text == "David"), NA),
                     Divine = NA,
                     Emi = ifelse("Emi" %in% input$order_order$text, which(input$order_order$text == "Emi"), NA),
                     Hala = NA,
                     Jeff = ifelse("Jeff" %in% input$order_order$text, which(input$order_order$text == "Jeff"), NA),
                     Kelsey = ifelse("Kelsey" %in% input$order_order$text, which(input$order_order$text == "Kelsey"), NA),
                     Marissa = ifelse("Marissa" %in% input$order_order$text, which(input$order_order$text == "Marissa"), NA),
                     Shannon = ifelse("Shannon" %in% input$order_order$text, which(input$order_order$text == "Shannon"), NA),
                     Zach = NA)
    return(df)
  })
  
  colors_data <- reactive({
    df %>% 
      filter(date != "2021-03-11" & date != "2021-03-12") %>% 
      tail(10) %>% 
      select(-c(Divine, Hala, Zach)) %>% 
      mutate(index = 1:length(date)) %>% 
      pivot_longer(Brian:Shannon) %>% 
      mutate(value = factor(value, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), ordered = T))  
  })
  
  todays_fun_fact <- reactive({
    ff_df <- tibble(date = input$date,
                    time = input$time,
                    funfact = input$fun_fact,
                    fun = input$fun_fact_fun,
                    fact = input$fun_fact_fact)
    return(ff_df)
  })
  observe(print(random_fun_fact()))
  random_fun_fact <- reactive({
    # ff <- fun_facts %>% 
    #   select(funfact)
    fact <- fun_facts[sample(nrow(fun_facts), 1), ]
    return(fact)
  })
  
  output$funfact <- renderUI({
    req(random_fun_fact())
    text <- random_fun_fact()$funfact
  })
    
    output$table_today <- renderDataTable({
      dt <- todays_order() %>% 
        select(-c(Divine, Hala, Zach))
      return(dt)
    }, options = list(dom = "t", ordering = F, 
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)
    
    output$heatmap <- renderPlot({
      who_rates %>% 
        filter(name != "Hala" & name != "Divine" & name != "Zach") %>% 
        filter(called_on != "Hala" & called_on != "Divine" & called_on != "Zach") %>% 
        filter(!is.na(total_shared_meetings)) %>% 
        ggplot(aes(x=called_on, y=name, fill=called_on_adj)) + 
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#5c9ad2", high = "#f59035",
                             midpoint = 0.14, limit = c(0, 0.4)) +
        theme_bw() +
        labs(y = "Person", x = "Calls On", fill = "frequency") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              text = element_text(size = 14, family = "Roboto"))
    })
    
    output$first <- renderPlot({
      ggplot(freq_first_last, aes(x = freq_first, y = reorder(name, freq_first))) +
        geom_bar(stat = "identity", fill = "#5c9ad2", size = 2) +
        labs(x = "% of meetings first",
             y = NULL) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              text = element_text(size = 14, family = "Roboto"))

    })
    
    output$last <- renderPlot({
      ggplot(freq_first_last, aes(x = freq_last, y = reorder(name, freq_last))) +
        geom_bar(stat = "identity", fill = "#5c9ad2", size = 2) +
        labs(x = "% of meetings last",
             y = NULL) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              text = element_text(size = 14, family = "Roboto"))

    })
    
    output$missing <- renderPlot({
      ggplot(freq_first_last, aes(x = freq_missing, y = reorder(name, freq_missing))) +
        geom_bar(stat = "identity", fill = "#5c9ad2", size = 2) +
        labs(x = "% of meetings missed",
             y = NULL) +
        theme_bw()+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              text = element_text(size = 14, family = "Roboto"))
    })
      
    
    output$hist_brian <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
    
    output$hist_jeff <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
    
    output$hist_marissa <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person == "Marissa") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "#f59035") +
        #facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_void()
      hist
    }, height = 100)
    
    output$hist_shannon <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
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
    
    output$colors_brian <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "Brian") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_carly <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "Carly") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_david <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "David") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_emi <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "Emi") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_jeff <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "Jeff") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_kelsey <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "Kelsey") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_marissa <- renderPlot({
      hist <- colors_data() %>% 
        filter(name == "Marissa") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
      hist
    }, height = 25)
    
    output$colors_shannon <- renderPlot({
      hist <- colors_data() %>% 
        mutate(value = factor(value, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), ordered = T)) %>% 
        filter(name == "Shannon") %>% 
        ggplot(aes(x = index, y = name, fill = value)) +
        geom_tile(color = "white", size = 1) + 
        scale_fill_manual(values = c("#ff0000", "#f59035", "#ffff00", "#149414", "#5c9ad2", "#2b5bb0", "#663399", "#ff69b4")) + 
        theme_void() +
        theme(legend.position = "none")
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
        select(-c(Divine, Hala, Zach))
      return(dt)
    }, options = list(dom = "t", ordering = F, pageLength = 20,
                      columnDefs = list(list(width = '100px', targets = "_all", className = "dt-center"))), 
    rownames = F)

    
    
})

