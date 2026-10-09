#Helper functions

# Fill colors for speaking positions 1..n: orange (went early) -> light gray -> navy (went late)
position_colors <- function(n) {
  colorRampPalette(c("#B35A00", "#FAA634", "#D8D4D2", "#2495B7", "#004E7B"), space = "Lab")(n)
}

# Label color for each fill: whichever of white or dark text reads better
label_colors <- function(fills) {
  ifelse(colorspace::contrast_ratio(fills, "white") >= colorspace::contrast_ratio(fills, "#262626"), "white", "#262626")
}

# One distinct color per active member, for the dependency wheel. Drawn from the brand's navy/blue/teal
# and orange/brown families, ordered so neighbors on the wheel stay distinguishable (including for
# colorblind viewers). Colors are reused only if the team grows past the length of this list
distinct_colors <- c("#006C64", "#936823", "#63B3FF", "#FAA634", "#004E7B", "#9BD3FF", "#2495B7",
                     "#5E2B00", "#1F74BF", "#D17236", "#00B9C3", "#833F27", "#31EBDF")
member_colors <- setNames(rep_len(distinct_colors, length(active_members)), active_members)

# Histogram of how often a person speaks in each position
make_order_hist <- function(.data, .filter_name) {
  .data %>%
    filter(name == .filter_name) %>%
    ggplot(aes(x = order)) +
    geom_bar(stat = "count", fill = "#004E7B") +
    scale_x_continuous(breaks = seq_len(max_position), limits = c(0.5, max_position + 0.5)) +
    theme_void()
}

make_colors_chart <- function(.data, .filter_name) {
  fills <- setNames(position_colors(nlevels(.data$value)), levels(.data$value))
  
  .data %>%
    filter(name == .filter_name) %>%
    ggplot(aes(x = index, y = name, fill = value)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = value, color = value), fontface = "bold", size = 5) +
    scale_fill_manual(values = fills, na.value = "transparent") +
    scale_color_manual(values = setNames(label_colors(fills), names(fills)), na.value = "transparent") +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill="transparent")
          )

}

# "Personal records" block on a person's card (nothing until they've been to a meeting)
personal_records_section <- function(.name) {
  r <- personal_records[personal_records$name == .name, ]
  if (nrow(r) == 0) return(NULL)

  first_text <- if (r$attended_this_month == 0) {
    "no games yet"
  } else {
    paste0(r$first_this_month, " of ", r$attended_this_month, ifelse(r$attended_this_month == 1, " game", " games"))
  }

  div(class="personal-records",
      h5("Personal records"),
      h5(paste0("Most consecutive games: ", r$longest_attendance_streak), align = "left"),
      h5(paste0("Led off in ", format(this_month, "%B"), ": ", first_text), align = "left")
  )
}

# Individual stats panel for one person; outputs are hist_<name> and colors_<name>
person_panel <- function(.name) {
  id <- tolower(.name)
  s <- stats[stats$name == .name, ]

  written_stats <- if (nrow(s) == 0) {
    h5("Not enough games yet!", align = "center")
  } else {
    tagList(
      h5(paste0("Usual spot in the lineup: ", s$mode_pretty),  align = "left"),
      h5(paste0("Most likely to call on: ", s$calls_on_most, " (", s$called_on_x_pct, "%)"), align = "left"),
      h5(paste0("Most likely to be called on by: ", s$called_on_by_most, " (", s$called_on_by_x_pct, "%)"), align = "left")
    )
  }

  # Jersey-number badge showing their usual spot in the lineup
  jersey <- if (nrow(s) > 0) div(class="jersey-number", title = "Usual spot in the lineup", s$mode)

  div(
    wellPanel(
      jersey,
      div(
        class="personal-stats-header",
        h3(.name)
      ),
      plotOutput(paste0("hist_", id), height = "100%", width= '70%'),
      div(class="indv-written-stats",
          written_stats
      ),
      personal_records_section(.name),
      div(class="recent-activity",
          h5("Last 10 games"),
          plotOutput(paste0("colors_", id), height = "100%")
      )
    )
  )
}

# "On this day" fun fact: the one closest to this date a year ago (within a week), preferring ones
# rated fun; falls back to two years ago, three years ago, ... Returns NULL if nothing is close
on_this_day_fact <- function(.facts, .today = Sys.Date(), .window = 7) {
  facts <- filter(.facts, !is.na(date))
  years_back <- seq_len(year(.today) - year(min(facts$date)))
  
  for (y in years_back) {
    target <- .today %m-% years(y)
    match <- facts %>% 
      mutate(days_off = abs(as.numeric(date - target))) %>% 
      filter(days_off <= .window) %>% 
      arrange(days_off, fun != "Yes!") %>% 
      slice_head(n = 1)
    if (nrow(match) == 1) return(match)
  }
  NULL
}
