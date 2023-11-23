# Theme for all figs
backg <- "#141415"
ggdark::dark_theme_gray() %+replace% 
  ggplot2::theme(
    plot.background = element_rect(color = backg, fill = backg),
    legend.box.background = element_rect(fill = backg, color = backg),
    legend.background = element_rect(fill = backg, color = backg),
    panel.grid = element_line(color = "gray20")
  ) %>%
  ggplot2::theme_set()

# If theme_set doesn't work...
theme_darkmode <- \ (backg = "#141415") {
  ggdark::dark_theme_gray() %+replace% 
    ggplot2::theme(
      plot.background = element_rect(color = backg, fill = backg),
      legend.box.background = element_rect(fill = backg, color = backg),
      legend.background = element_rect(fill = backg, color = backg),
      panel.grid = element_line(color = "gray20")
    )
}