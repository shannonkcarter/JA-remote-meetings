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



make_colors_chart <- function(.data, .filter_name) {
  .data %>% 
    filter(name == .filter_name) %>% 
    ggplot(aes(x = index, y = name, fill = value)) +
    geom_tile(color = "white", size = 1) + 
    geom_text(aes(label=value), family="Roboto", fontface="bold") +
    scale_fill_manual(values = c(`1`="#ff0000", `2`="#f59035", `3`="#ffff00", 
                                 `4`="#149414", `5`="#5c9ad2", `6`="#2b5bb0", `7`="#663399", `8`="#ff69b4")) + 
    theme_void() +
    theme(legend.position = "none", 
          panel.background = element_rect(fill="transparent")
          ) 
    
}