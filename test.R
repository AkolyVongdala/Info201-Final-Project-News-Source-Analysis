library(shiny)
library(ggplot2)
library(rsconnect)
library(rlang)
data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)

data$share_count
data$reaction_count
data$comment_count


plot <- ggplot(data = data, aes(x = Rating, width = .5)) + 
  geom_bar()

plot
