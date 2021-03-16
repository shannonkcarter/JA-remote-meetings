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
        
        removeModal()
        #shinyjs::reset("form")
        #shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
        
      } 
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

