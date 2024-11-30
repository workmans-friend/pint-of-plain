library(ggtext)
library(dplyr)
library(ggplot2)

words <- c(
  "When things go wrong",
  "and will not come right,",
  "Though you do the best",
  "you can, When life looks",
  "black as the hour of night",
  "A pint of plain is your only",
  "man. When money’s tight",
  "and hard to get, And your",
  "horse has also ran, When",
  "all you have is a heap of debt",
  "A pint of plain is your only man",
  "When health is bad and your heart",
  "feels strange, And your face is",
  "pale and wan, When doctors say",
  "you need a change–A pint of plain",
  "is your only man. When food is scarce",
  "and your larder bare, And no rashers",
  "grease your pan, When hunger grows",
  "as your meals are rare–A pint of plain",
  "is your only man. In time of trouble and",
  "lousey strife, You have still got a darlint",
  "plan, You still can turn to a brighter life",
  "A pint of plain is your only man",
  "The Workman's Friend",
  "",
  "")


usual <- function(n) {

  df <- data.frame(
    words = words,
    order = 1:length(words),
    size = c(rep('a1', length(words) - 4), 'a2', rep('a3', 2), 'a4'),
    pour = c(rep('a', n-1), 'b', rep('c', length(words) - n))
  )

  return(df)
}


plot <- function(plotData) {

p <- ggplot(plotData,
         aes(y = order, x = 1,
             label = words, size = size,
             color = factor(pour, levels = c('a', 'b', 'c')))) +
    geom_textbox(
      fill = "transparent",
      box.colour = 'transparent',
      halign = 0.5,
      width = 1
    ) +
    scale_y_continuous(limits = c(1, length(words))) +
    scale_size_manual(values = c(8, 10, 13.5, 17.5)/.pt) +
    scale_color_manual(values = list('a'="grey30", 'b'="#664E4C", 'c'="black")) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "black", color='black'),
      panel.background = element_rect(fill = "black", color='black'),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )

return(p)

}
