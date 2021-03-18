#Helper functions

#ggplot bar charts
make_bar_chart <- function(.data, .x_var, .y_var, .x_axis_name) {
  ggplot(freq_first_last, aes(x = freq_first, y = reorder(name, freq_first))) +
    geom_bar(stat = "identity", fill = "#5c9ad2", size = 2) +
    geom_text(aes(label = paste0(round(freq_first), "%")), hjust=1, family="Roboto", fontface="bold") +
    labs(x = "% of meetings first",
         y = NULL) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12, family = "Roboto"),
          text = element_text(size = 12, family = "Roboto")
          )
}

