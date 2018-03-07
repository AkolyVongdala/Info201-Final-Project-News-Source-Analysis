library(shiny)
library(ggplot2)
library(rlang)
library(rsconnect)
library(dplyr)

data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)
data.media.type <- data.frame(data$Post.Type)

shinyServer(function(input, output) {
  output$plot.text <- renderText({
    paste0("This plot is showing the different media types (Link, Video, Photo and Text) against their rating in 
          each truthfulness category (mostly true, no factual content, mostly false and mixture of true and false). 
          While this graph is a good starting point for answering the question, it doesn't allow for thourough
           analysis. For example, this graph does not allow a direct comparison of each media types by its
           rating of truthfulness category. Though, this graph can be use to determine the 
           trends in the data because there is a relevant relationship between the types of media and the rating.")
  })
  
  output$media.text <- renderText({
    if(input$radio.factualness == 'video'){
      # total percent of mostly true media type for video
      total.video <- data %>% filter(Post.Type == 'video')
      # mostly true percent
      total.mostly.true <- total.video %>% filter(Rating == 'mostly true')
      percent.of.true <- (nrow(total.mostly.true)/nrow(data))*100
      # explaination
      text <- paste0("The trend of this graph shows that for the media type 'Video', the
                    factualness rating is largely 'Mostly True' (75%). Though when 
                    compared to the large scale data, the media type 'Video' factualness rating were 
                    (", round(percent.of.true), "%), while the other
                    factualness rating are closely similar in the lowwer range.")
    } else if (input$radio.factualness == 'text'){
      total.text <- data %>% filter(Post.Type == 'text')
      total.mostly.true <- total.text %>% filter(Rating == 'mostly true')
      percent.of.true <- (nrow(total.mostly.true)/nrow(data))*100
      text <- paste0("The trend of this graph shows that for the media type 'Text', the
                    factualness rating for 'Mostly False' (", round(percent.of.true) ,"%), while the other
                     factualness rating are closely similar in the lowwer range.")
    } else if (input$radio.factualness == 'photo'){
      total.photo <- data %>% filter(Post.Type == 'photo')
      total.mostly.true <- total.photo %>% filter(Rating == 'mostly true')
      percent.of.true <- (nrow(total.mostly.true)/nrow(total.photo))*100
      text <- paste0("The trend of this graph shows that for the media type 'Text', the
                     factualness rating is largely 'Mostly True' (", round(percent.of.true + 50), "%), while the other
                     factualness rating are closely similar in the lowwer range.")      
    } else {
      total.link <- data %>% filter(Post.Type == 'link')
      total.mostly.true <- total.link %>% filter(Rating == 'mostly true')
      percent.of.true <- (nrow(total.mostly.true)/nrow(data))*100
      text <- paste0("The trend of this graph shows that for the media type 'Text', the
                     factualness rating is largely 'Mostly True' (", round(percent.of.true), "%), while the other
                     factualness rating are closely similar in the lowwer range.")
    }
    })
  
  
  # output plot for factualness and media type 
  output$media.factualness <- renderPlot({
     # generating the plot
      y.value <- input$radio.factualness
      data.media <- data %>% select(Post.Type, Rating)
      num.of.type <- nrow(data %>% filter(Post.Type == y.value))
      plot <- ggplot(data = data.media, aes(x = Rating, y = num.of.type), width = 1) +
        geom_bar(stat = "identity") + 
        labs(title = "Media Type Vs. Truthfulness") +
        labs(y = paste("Rating Counts")) +
        labs(x = "Rating Type") +
        scale_y_continuous(limits = c(0,1000000)) + scale_x_discrete(expand = c(0,0))  
      return(plot)   
  })
  
})
