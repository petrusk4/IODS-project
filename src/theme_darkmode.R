theme_darkmode <- \ (backg = "#141415") {
  ggdark::dark_theme_gray() %+replace% 
    ggplot2::theme(
      plot.background = element_rect(color = backg, fill = backg),
      legend.box.background = element_rect(fill = backg, color = backg),
      legend.background = element_rect(fill = backg, color = backg),
      panel.grid = element_line(color = "gray20")
    )
}