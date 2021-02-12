
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
    }, options= list(dom = "t"), rownames = F)
    
    output$hists <- renderPlot({
      hist <- data %>% 
        pivot_longer(cols = Brian:Zach, names_to = "person", values_to = "order") %>% 
        mutate(order = as.numeric(order)) %>% 
        filter(person != "Hala" & person != "Divine" & person != "Zach") %>%
        filter(order < 9) %>% 
        ggplot(aes(x = order)) +
        geom_bar(stat = "count", fill = "dodgerblue") +
        facet_wrap(~person) +
        scale_x_continuous(breaks = seq(1, 8, 1))
      hist
    })
})
