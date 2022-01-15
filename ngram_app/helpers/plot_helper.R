build_empty_plot <- function(text) {
  renderPlot({
    ggplot() +
      annotate(
        "text",
        x = 4,
        y = 25,
        size = 8,
        label = text
      ) +
      theme_void()
  })
}

build_ngram_line_plot <- function(df) {
  # build line plot
  plot <- res %>%
    group_by(year) %>%
    ggplot(aes(
      x = year,
      y = log(freq, base = 10),
      group = ngram,
      color = ngram
    )) +
    geom_smooth(method = "loess", se = FALSE)
  
  # Set axis styles
  plot <- plot + theme(
    axis.text.x = element_text(
      size = 14
    ),
    axis.title.x = element_text(
      color = "darkgrey",
      size = 12,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "darkgrey",
      size = 12,
      face = "bold"
    )
  )
  
  # Set axis labels
  plot <- plot +
    ylab('Log ( frequency of word per year )')
  
  
  return(plot)
}