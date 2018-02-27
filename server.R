library(shiny)
library(ggplot2)
library(rlang)
library(rsconnect)

data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  output$popularity.plot <- renderPlot({
    y.value <- input$popularity.radio
    plot <- ggplot(data = data, aes(x = Rating, y = data[, y.value], width = .5)) + 
      geom_bar(stat = "identity") +
      labs(y = rlang::sym(y.value)) +
      geom_text(label = sum(data[, y.value]), na.rm = TRUE)
    return(plot)
  })
})