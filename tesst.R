library(shiny)
library(ggplot2)
library(rlang)
library(rsconnect)
library(tidyr)
library(cowplot)
install.packages("plotly")

data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)


data.long <- gather(data, key = count_type, value = counts, share_count, reaction_count, comment_count)
comment.data <- dplyr::filter(data.long, count_type == "comment_count")
comment.plot <- ggplot(data = comment.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(fill = "Count Type") +
  scale_fill_manual(values = "#66c2a5")  


reaction.data <- dplyr::filter(data.long, count_type == "reaction_count")
reaction.plot <- ggplot(data = reaction.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(fill = "Count Type") +
  scale_fill_manual(values = "#fc8d62")

share.data <- dplyr::filter(data.long, count_type == "share_count")
share.plot <- ggplot(data = share.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(fill = "Count Type") +
  scale_fill_manual(values = "#8da0cb")

plot_grid(comment.plot, reaction.plot, share.plot, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)
