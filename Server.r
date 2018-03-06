library(shiny)
library(ggplot2)
library(rlang)
library(rsconnect)
library(tidyr)
library(dplyr)
library(cowplot)

data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)
data.long <- gather(data, key = count_type, value = counts, share_count, reaction_count, comment_count)

# Adds a column to data long that is the counts per post
rows1 <- nrow(dplyr::filter(data.long, Rating == "mixture of true and false"))
per.post1 <- dplyr::filter(data.long, Rating == "mixture of true and false")$counts / rows1

rows2 <- nrow(dplyr::filter(data.long, Rating == "mostly false"))
per.post2 <- dplyr::filter(data.long, Rating == "mostly false")$counts / rows2

rows3 <- nrow(dplyr::filter(data.long, Rating == "mostly true"))
per.post3 <- dplyr::filter(data.long, Rating == "mostly true")$counts / rows3

rows4 <- nrow(dplyr::filter(data.long, Rating == "no factual content"))
per.post4 <- dplyr::filter(data.long, Rating == "no factual content")$counts / rows4

data.long <- arrange(data.long, Rating) # Makes alphabetical

# NEEDS to be put in in alphabetical order
data.long$per.post <- c(per.post1, per.post2, per.post3, per.post4) 


shinyServer(function(input, output) {
  
  types.to.display <- reactive({
    if (length(input$popularity.types) == 1) {
      data.long <- dplyr::filter(data.long, count_type == input$popularity.types[1])
    } else if (length(input$popularity.types) == 2) {
      data.long <- dplyr::filter(data.long, count_type == input$popularity.types[1] | count_type == input$popularity.types[2])
    } else  if (length(input$popularity.types) == 3){
      data.long <- dplyr::filter(data.long, count_type == input$popularity.types[1] | count_type == input$popularity.types[2] | count_type == input$popularity.types[3])
    }
    
    return(data.long)
  })

  
  output$popularity.plot <- renderPlot({
    data.long <- types.to.display()
    
    plot <- ggplot(data = data.long, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(fill = "Count Type") +
      scale_fill_manual(values = c("share_count" = "#66c2a5", 
                                   "reaction_count" = "#fc8d62", 
                                   "comment_count" = "#8da0cb"))
    return(plot)
  })
  
  output$popularity.percents <- renderPlot({
    data.long <- types.to.display()
    
    plot <- ggplot(data = data.long, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(fill = "Count Type", y = "Counts per Post") +
      scale_fill_manual(values = c("share_count" = "#66c2a5", 
                                   "reaction_count" = "#fc8d62", 
                                   "comment_count" = "#8da0cb"))
    return(plot)
  })  
  
  output$popularity.grid <- renderPlot({
    
    comment.data <- dplyr::filter(data.long, count_type == "comment_count")
    reaction.data <- dplyr::filter(data.long, count_type == "reaction_count")
    share.data <- dplyr::filter(data.long, count_type == "share_count")
    
    # Plots for counts
    comment.plot <- ggplot(data = comment.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Comments Counts") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#8da0cb")  
    
    reaction.plot <- ggplot(data = reaction.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Reactions Counts") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#fc8d62")
    
    share.plot <- ggplot(data = share.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Shares Counts") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") + 
      scale_fill_manual(values = "#66c2a5")
    
    # Plots for counts per post
    comment.plot.per <- ggplot(data = comment.data, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Comments Counts per Post") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#8da0cb")  
    
    reaction.plot.per <- ggplot(data = reaction.data, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Reactions Counts per Post") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#fc8d62")
    
    share.plot.per <- ggplot(data = share.data, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Shares Counts per Post") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") + 
      scale_fill_manual(values = "#66c2a5")
    
    grid <- plot_grid(comment.plot, reaction.plot, share.plot, 
                      comment.plot.per, reaction.plot.per, share.plot.per, ncol = 3, nrow = 2)   
    return(grid)
  })
  
  popularity.types <- reactive({
    length = length(input$popularity.types)
    if (length == 3 || length == 0) {
      types <- "comment_count, reaction_count, share_count"
    } else if (length == 1) {
      types <- input$popularity.types[1]
    } else {
      types <- paste0(input$popularity.types[1], ", ", input$popularity.types[2])
    }
    return(types)
  })
  
  output$popularity.text <- renderText({
    paste0("This first plot is showing different popularity types (", 
           popularity.types(), ") against their counts in each truthfulness category. While this graph is a
           good starting point for answering the question, it doesn't allow for thourough
           analysis. For example, there are ", rows1, " posts in the \"mixture of true and false\" category, 
           but ", rows3, " posts in the \"mostly true\" category. This graph can't be used to determine any 
           trends in the data because the counts depend on the number of posts. To eliminate that problem, 
           the next graph looks at the counts of the different popularity types per post.")
  })
  
  output$popularity.per.text <- renderText({
    paste0("This bar graph shows the different popularity types (", popularity.types(),
           ") against their counts per post in each truthfulness category. This graph can be
           used to determine trends since it is showing the averages ", popularity.types(),
           " per post. ")
  })
  
})