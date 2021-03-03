
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  data <- read_csv(here::here("standup_data.csv"))
  
  #observe(print(str(input$order_order)))
  #observe(print(which(input$order_order == "David")))
    todays_order <- reactive({
      df <- data.frame(date = input$date,
                 time = input$time,
                 Brian = ifelse("Brian" %in% input$order_order, which(input$order_order == "Brian"), NA),
                 Carly = ifelse("Carly" %in% input$order_order, which(input$order_order == "Carly"), NA),
                 David = ifelse("David" %in% input$order_order, which(input$order_order == "David"), NA),
                 Divine = NA,
                 Emi = ifelse("Emi" %in% input$order_order, which(input$order_order == "Emi"), NA),
                 Hala = NA,
                 Jeff = ifelse("Jeff" %in% input$order_order, which(input$order_order == "Jeff"), NA),
                 Kelsey = ifelse("Kelsey" %in% input$order_order, which(input$order_order == "Kelsey"), NA),
                 Marissa = ifelse("Marissa" %in% input$order_order, which(input$order_order == "Marissa"), NA),
                 Shannon = ifelse("Shannon" %in% input$order_order, which(input$order_order == "Shannon"), NA),
                 Zach = NA)
      return(df)
    })
    
    output$todays_order <- renderDataTable({
      dt <- todays_order() %>% 
        select(-c(Divine, Hala, Zach))
      return(dt)
    }, options = list(dom = "t", ordering = F, columnDefs = list(list(width = '150px', targets = 2:9, className = "dt-center"))), rownames = F)
    
    output$hists <- renderPlot({
      hist <- df %>% 
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person != "Hala" & person != "Divine" & person != "Zach") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "dodgerblue") +
        facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1)) + 
        theme_ja()
      hist
    })
    
    #Submit password
    observeEvent(input$submit, {
      showModal(
        modalDialog(
          title = "Enter password to submit data",
          textInput(inputId = "password_input", "Type password"),
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
        s3saveRDS(input_data(),
                  bucket = "standapp",
                  object = "standapp-data.rds")
        
        removeModal()
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
        
      } 
    })
})

