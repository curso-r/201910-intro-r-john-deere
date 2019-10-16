library(tidyverse)
library(ggQC)
library(plotly)

# Setup Data --------------------------------------------------------------
df <- data.frame(
  x = letters[1:10],
  y = as.integer(runif(n = 10, min = 0, max=100))
)

# Render Pareto Plot ------------------------------------------------------

df %>%
ggplot(aes(x = x, y = y)) +
  stat_pareto(
    point.color = "salmon",
    point.size = 3,
    line.color = "grey",
    bars.fill = c("royalblue", "orange"),
  )
