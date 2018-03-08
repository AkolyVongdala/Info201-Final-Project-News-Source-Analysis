library(shiny)
library(ggplot2)
library(rlang)
library(rsconnect)
library(tidyr)
library(dplyr)
library(cowplot)

data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)

############# QUESTION ONE CODE ############################
# Makes data long, organizes all popularity counts into one column
data.long <- gather(data, key = count_type, value = counts, share_count, reaction_count, comment_count)

# Removes any data with "NA" count values
data.long <- dplyr::filter(data.long, !is.na(counts))

# Removes any data with "NA" rating values
data.long <- data.long <- dplyr::filter(data.long, !is.na(Rating) & Rating != "")

# Adds a column to data long that is the counts per post
rows1 <- nrow(dplyr::filter(data, Rating == "mixture of true and false")) # Using data to get actual # of posts
per.post1 <- dplyr::filter(data.long, Rating == "mixture of true and false")$counts / rows1

rows2 <- nrow(dplyr::filter(data, Rating == "mostly false"))
per.post2 <- dplyr::filter(data.long, Rating == "mostly false")$counts / rows2

rows3 <- nrow(dplyr::filter(data, Rating == "mostly true"))
per.post3 <- dplyr::filter(data.long, Rating == "mostly true")$counts / rows3

rows4 <- nrow(dplyr::filter(data, Rating == "no factual content"))
per.post4 <- dplyr::filter(data.long, Rating == "no factual content")$counts / rows4

data.long <- arrange(data.long, Rating) # Makes alphabetical

# NEEDS to be put in in alphabetical order
data.long$per.post <- c(per.post1, per.post2, per.post3, per.post4) 


shinyServer(function(input, output) {
  
  # Different popularity types to display
  types.to.display <- reactive({
    if (length(input$popularity.types) == 1) {
      data.long <- dplyr::filter(data.long, count_type == input$popularity.types[1])
    } else if (length(input$popularity.types) == 2) {
      data.long <- dplyr::filter(data.long, count_type == input$popularity.types[1] 
                                 | count_type == input$popularity.types[2])
    } else  if (length(input$popularity.types) == 3){
      data.long <- dplyr::filter(data.long, count_type == input$popularity.types[1] 
                                 | count_type == input$popularity.types[2] 
                                 | count_type == input$popularity.types[3])
    }
    
    return(data.long)
  })

  # Renders popularity plot, shows counts
  output$popularity.plot <- renderPlot({
    data.long <- types.to.display()
    
    plot <- ggplot(data = data.long, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(fill = "Count Type") +
      scale_fill_manual(values = c("share_count" = "#679289", 
                                   "reaction_count" = "#F4C095", 
                                   "comment_count" = "#EE2E31"))
    return(plot)
  })
  
  # Renders popularity plot, shows counts per post
  output$popularity.percents <- renderPlot({
    data.long <- types.to.display()
    
    plot <- ggplot(data = data.long, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(fill = "Count Type", y = "Counts per Post") +
      scale_fill_manual(values = c("share_count" = "#679289", 
                                   "reaction_count" = "#F4C095", 
                                   "comment_count" = "#EE2E31"))
    return(plot)
  })  
  
  # Renders individual plots for popularity types includes both counts and counts per post
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
      scale_fill_manual(values = "#EE2E31")  
    
    reaction.plot <- ggplot(data = reaction.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Reactions Counts") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#F4C095")
    
    share.plot <- ggplot(data = share.data, aes(x = Rating, y = counts, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Shares Counts") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") + 
      scale_fill_manual(values = "#679289")
    
    # Plots for counts per post
    comment.plot.per <- ggplot(data = comment.data, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Comments Counts per Post") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#EE2E31")  
    
    reaction.plot.per <- ggplot(data = reaction.data, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Reactions Counts per Post") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") +
      scale_fill_manual(values = "#F4C095")
    
    share.plot.per <- ggplot(data = share.data, aes(x = Rating, y = per.post, width = .5, fill = count_type)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Shares Counts per Post") +
      scale_x_discrete(labels = c("T&F", "MF", "MT", "NFC")) +
      theme(legend.position="none") + 
      scale_fill_manual(values = "#679289")
    
    grid <- plot_grid(comment.plot, reaction.plot, share.plot, 
                      comment.plot.per, reaction.plot.per, share.plot.per, ncol = 3, nrow = 2)   
    return(grid)
  })
  
  # Reactive text that gets the names of the popularity types that are being displayed
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
  
  # Renders text description for popularity plot of counts
  output$popularity.text <- renderText({
    paste0("This first plot is showing different popularity types (", 
           popularity.types(), ") against their counts in each truthfulness category. While this graph is a
           good starting point for answering the question, it doesn't allow for thorough
           analysis. For example, there are ", rows1, " posts in the \"mixture of true and false\" category, 
           but ", rows3, " posts in the \"mostly true\" category. This graph can't be used to determine any 
           trends in the data because the counts depend on the number of posts. To eliminate that problem, 
           the next graph looks at the counts of the different popularity types per post.")
  })
  
  # Renders text description for popularity plot of counts per post
  output$popularity.per.text <- renderText({
    paste0("This bar graph shows the different popularity types (", popularity.types(),
           ") against their counts per post in each truthfulness category. This graph can be
           used to determine trends since it is showing the averages ", popularity.types(),
           " per post. From this graph it is evident that \"mostly true\" posts are less popular, meaning 
           that they are shared, comment on, and reacted to less. The most popular posts are the ones 
           that contain \"no factual content\".")
  })

  output$popularity.grid.text <- renderText({
    paste("These graphs are displaying all the data shown on the previous two graphs but 
          broken down into their individual popularity types.")
  })
  
  ############# QUESTION 2 CODE #########################
  
  # creates a reactive data set that filters the data by the user's
  # inputs on category
  filtered.category <- reactive({
    rating <- input$Rating
    category <- input$category
    
    if (category != "all") {
      data <- data %>% filter(Category == category)
    }
    
    return(data)
    
  })
  
  # creates a reactive data set that filters the data by the user's
  # inputs on category AND rating
  filtered <- reactive({
    rating <- input$Rating
    category <- input$category
    
    if (category != "all") {
      data <- data %>% filter(Category == category)
    }
    if (rating != "all") {
      data <-
        data %>% filter(Rating == rating) %>% select(Category, Rating, Page)
    }
    return(data)
  })
  
  # this table takes in account the users widgets and ensures
  # it gives a correct table of rating percentages for different
  # news categories which can be filtered by rating type or
  # catagory name
  output$category.table <- renderTable(rownames = TRUE, {
    rating <- input$Rating
    category <- input$category
    
    # This function creates a percentage table of
    # ratings for whatever category is passed in
    table.making <- function(category) {
      data <- data %>% filter(Category == category)
      
      total <- nrow(data)
      
      total.percentage <- '100%'
      
      if (rating != "all") {
        no.factual.content <- nrow(data %>% filter(Rating == rating))
        
        rating.percentage <-
          paste(round((no.factual.content / total) * 100), "%", sep = "")
        all.percentages <-
          data.frame(rating.percentage, stringsAsFactors = FALSE)
        colnames(all.percentages) <- c(rating)
        table <- all.percentages
        return(all.percentages)
        
      } else {
        no.factual.content <-
          nrow(data %>% filter(Rating == 'no factual content'))
        
        nfc.percentage <-
          paste(round((no.factual.content / total) * 100), "%", sep = "")
        
        mostly.true <-
          nrow(data %>% filter(Rating == 'mostly true'))
        
        true.percentage <-
          paste(round((mostly.true / total) * 100), "%", sep = "")
        
        mixture.tandf <-
          nrow(data %>% filter(Rating == 'mixture of true and false'))
        
        tandf.percentage <-
          paste(round((mixture.tandf / total) * 100), "%", sep = "")
        
        mostly.false <-
          nrow(data %>% filter(Rating == 'mostly false'))
        
        false.percentage <-
          paste(round((mostly.false / total) * 100), "%", sep = "")
        
        all.percentages <-
          data.frame(
            tandf.percentage,
            false.percentage,
            true.percentage,
            nfc.percentage,
            total.percentage,
            stringsAsFactors = FALSE
          )
        
        colnames(all.percentages) <-
          c(
            "mixture of true and false",
            "mostly false",
            "mostly true",
            'no factual content',
            "total"
          )
        return(all.percentages)
      }
      
    }
    
    # checks to see if one or all categories have been
    # selected by the user. calls function above to
    # create truth rating percentage tables for each
    # category
    if (category != "all") {
      rows <- c(paste("Percentage of ", category, " articles"))
      table.one <- table.making(category)
      row.names(table.one) <- rows
      
      return(table.one)
    } else {
      table.one <- table.making('mainstream')
      table.two <- table.making('left')
      table.three <- table.making('right')
      table.one[nrow(table.one) + 1,] <- table.two
      table.one[nrow(table.one) + 1,] <- table.three
      rows <-
        c(
          'Percentage of mainstream articles',
          'Percentage of left articles',
          'Percentage of right articles'
        )
      
      row.names(table.one) <- rows
      table.category <- table.one
      return(table.one)
      
      
    }
    
    # filters by rating if another rating besides
    # all of the ratings was selected
    if (rating != "all") {
      data <-
        data %>% filter(Rating == rating) %>% select(Category, Rating, Page)
      
    }
    return(data)
    
    
  })
  
  # creates a reactive (but static) table of all
  # the rating percentages for each category to pull
  # information from in the future, such as the reactive
  # text output
  table.category <- reactive({
    # Again, this function creates a percentage table of
    # ratings for whatever category is passed in
    table.making <- function(category) {
      data <- data %>% filter(Category == category)
      
      total <- nrow(data)
      
      total.percentage <- '100%'
      no.factual.content <-
        nrow(data %>% filter(Rating == 'no factual content'))
      
      nfc.percentage <-
        paste(round((no.factual.content / total) * 100), "%", sep = "")
      
      mostly.true <-
        nrow(data %>% filter(Rating == 'mostly true'))
      
      true.percentage <-
        paste(round((mostly.true / total) * 100), "%", sep = "")
      
      mixture.tandf <-
        nrow(data %>% filter(Rating == 'mixture of true and false'))
      
      tandf.percentage <-
        paste(round((mixture.tandf / total) * 100), "%", sep = "")
      
      mostly.false <-
        nrow(data %>% filter(Rating == 'mostly false'))
      
      false.percentage <-
        paste(round((mostly.false / total) * 100), "%", sep = "")
      
      all.percentages <-
        data.frame(
          tandf.percentage,
          false.percentage,
          true.percentage,
          nfc.percentage,
          total.percentage,
          stringsAsFactors = FALSE
        )
      
      colnames(all.percentages) <-
        c(
          "mixture of true and false",
          "mostly false",
          "mostly true",
          'no factual content',
          "total"
        )
      return(all.percentages)
    }
    
    # combines all the tables together to
    # one table in order to return. also
    # renames the rows
    table.one <- table.making('mainstream')
    table.two <- table.making('left')
    table.three <- table.making('right')
    table.one[nrow(table.one) + 1,] <- table.two
    table.one[nrow(table.one) + 1,] <- table.three
    rows <-
      c(
        'Percentage of mainstream articles',
        'Percentage of left articles',
        'Percentage of right articles'
      )
    
    row.names(table.one) <- rows
    return(table.one)
    
    
  })
  
  #
  output$page.table <- renderTable(rownames = TRUE, {
    rating <- input$Rating
    category <- input$category
    
    
    table.making <- function(page) {
      data <- data %>% filter(Page == page)
      
      total <- nrow(data)
      
      total.percentage <- '100%'
      
      if (rating != "all") {
        no.factual.content <- nrow(data %>% filter(Rating == rating))
        
        rating.percentage <-
          paste(round((no.factual.content / total) * 100), "%", sep = "")
        all.percentages <-
          data.frame(rating.percentage, stringsAsFactors = FALSE)
        colnames(all.percentages) <- c(rating)
        
        return(all.percentages)
        
      } else {
        no.factual.content <-
          nrow(data %>% filter(Rating == 'no factual content'))
        
        nfc.percentage <-
          paste(round((no.factual.content / total) * 100), "%", sep = "")
        
        mostly.true <-
          nrow(data %>% filter(Rating == 'mostly true'))
        
        true.percentage <-
          paste(round((mostly.true / total) * 100), "%", sep = "")
        
        mixture.tandf <-
          nrow(data %>% filter(Rating == 'mixture of true and false'))
        
        tandf.percentage <-
          paste(round((mixture.tandf / total) * 100), "%", sep = "")
        
        mostly.false <-
          nrow(data %>% filter(Rating == 'mostly false'))
        
        false.percentage <-
          paste(round((mostly.false / total) * 100), "%", sep = "")
        
        all.percentages <-
          data.frame(
            tandf.percentage,
            false.percentage,
            true.percentage,
            nfc.percentage,
            total.percentage,
            stringsAsFactors = FALSE
          )
        
        colnames(all.percentages) <-
          c(
            "mixture of true and false",
            "mostly false",
            "mostly true",
            'no factual content',
            "total"
          )
        
        return(all.percentages)
      }
      
      
      
    }
    
    mainstream.page.one <- table.making("ABC News Politics")
    mainstream.page.two <- table.making("CNN Politics")
    mainstream.page.three <- table.making("Politico")
    
    mainstream.page.one[nrow(mainstream.page.one) + 1,] <-
      mainstream.page.two
    mainstream.page.one[nrow(mainstream.page.one) + 1,] <-
      mainstream.page.three
    
    mainstream.rows <-
      c("ABC News Politics", "CNN Politics", "Politico")
    row.names(mainstream.page.one) <- mainstream.rows
    
    left.page.one <- table.making("Addicting Info")
    left.page.two <- table.making("Occupy Democrats")
    left.page.three <- table.making("The Other 98%")
    
    left.page.one[nrow(left.page.one) + 1,] <- left.page.two
    left.page.one[nrow(left.page.one) + 1,] <- left.page.three
    
    left.rows <-
      c("Addicting Info", "Occupy Democrats", "The Other 98%")
    
    row.names(left.page.one) <- left.rows
    
    
    right.page.one <- table.making("Eagle Rising")
    right.page.two <- table.making("Freedom Daily")
    right.page.three <- table.making("Right Wing News")
    
    right.page.one[nrow(right.page.one) + 1,] <- right.page.two
    right.page.one[nrow(right.page.one) + 1,] <- right.page.three
    
    right.rows <-
      c("Eagle Rising", "Freedom Daily", "Right Wing News")
    row.names(right.page.one) <- right.rows
    
    
    if (category == "mainstream") {
      return(mainstream.page.one)
    } else if (category == "left") {
      return(left.page.one)
    } else if (category == "right") {
      return(right.page.one)
    } else {
      all.pages <- mainstream.page.one
      all.pages <- full_join(all.pages, left.page.one)
      all.pages <- full_join(all.pages, right.page.one)
      all.rows <-
        c(
          "ABC News Politics",
          "CNN Politics",
          "Politico",
          "Addicting Info",
          "Occupy Democrats",
          "The Other 98%",
          "Eagle Rising",
          "Freedom Daily",
          "Right Wing News"
        )
      row.names(all.pages) <- all.rows
      page.table <- all.pages
      
      return(all.pages)
      
    }
    if (rating != "all") {
      data <-
        data %>% filter(Rating == rating) %>% select(Category, Rating, Page)
      
      
    }
    return(data)
    
    
  })
  
  # static page table that will allow us to pull data
  # from when making conclusions or for render text output
  table.page <- reactive({
    table.making <- function(page) {
      data <- data %>% filter(Page == page)
      
      total <- nrow(data)
      
      total.percentage <- '100%'
      
      no.factual.content <-
        nrow(data %>% filter(Rating == 'no factual content'))
      
      nfc.percentage <-
        paste(round((no.factual.content / total) * 100), "%", sep = "")
      
      mostly.true <-
        nrow(data %>% filter(Rating == 'mostly true'))
      
      true.percentage <-
        paste(round((mostly.true / total) * 100), "%", sep = "")
      
      mixture.tandf <-
        nrow(data %>% filter(Rating == 'mixture of true and false'))
      
      tandf.percentage <-
        paste(round((mixture.tandf / total) * 100), "%", sep = "")
      
      mostly.false <-
        nrow(data %>% filter(Rating == 'mostly false'))
      
      false.percentage <-
        paste(round((mostly.false / total) * 100), "%", sep = "")
      
      all.percentages <-
        data.frame(
          tandf.percentage,
          false.percentage,
          true.percentage,
          nfc.percentage,
          total.percentage,
          stringsAsFactors = FALSE
        )
      
      colnames(all.percentages) <-
        c(
          "mixture of true and false",
          "mostly false",
          "mostly true",
          'no factual content',
          "total"
        )
      
      return(all.percentages)
    }
    
    mainstream.page.one <- table.making("ABC News Politics")
    mainstream.page.two <- table.making("CNN Politics")
    mainstream.page.three <- table.making("Politico")
    
    mainstream.page.one[nrow(mainstream.page.one) + 1,] <-
      mainstream.page.two
    mainstream.page.one[nrow(mainstream.page.one) + 1,] <-
      mainstream.page.three
    
    mainstream.rows <-
      c("ABC News Politics", "CNN Politics", "Politico")
    row.names(mainstream.page.one) <- mainstream.rows
    
    left.page.one <- table.making("Addicting Info")
    left.page.two <- table.making("Occupy Democrats")
    left.page.three <- table.making("The Other 98%")
    
    left.page.one[nrow(left.page.one) + 1,] <- left.page.two
    left.page.one[nrow(left.page.one) + 1,] <- left.page.three
    
    left.rows <-
      c("Addicting Info", "Occupy Democrats", "The Other 98%")
    
    row.names(left.page.one) <- left.rows
    
    
    right.page.one <- table.making("Eagle Rising")
    right.page.two <- table.making("Freedom Daily")
    right.page.three <- table.making("Right Wing News")
    
    right.page.one[nrow(right.page.one) + 1,] <- right.page.two
    right.page.one[nrow(right.page.one) + 1,] <- right.page.three
    
    right.rows <-
      c("Eagle Rising", "Freedom Daily", "Right Wing News")
    row.names(right.page.one) <- right.rows
    
    all.pages <- mainstream.page.one
    all.pages <- full_join(all.pages, left.page.one)
    all.pages <- full_join(all.pages, right.page.one)
    all.rows <-
      c(
        "ABC News Politics",
        "CNN Politics",
        "Politico",
        "Addicting Info",
        "Occupy Democrats",
        "The Other 98%",
        "Eagle Rising",
        "Freedom Daily",
        "Right Wing News"
      )
    row.names(all.pages) <- all.rows
    page.table <- all.pages
    
    return(all.pages)
  })
  
  output$newplot <- renderPlot({
    data <- filtered.category()
    
    return(ggplot(data = data, aes(x = Category, fill = Rating)) +
             geom_bar(stat = "count", position = position_dodge())+
             scale_fill_manual(values = c("mixture of true and false" = "#328BD3", 
                                          "mostly false" = "#0B5563", 
                                          "mostly true" = "#A2BCE0",
                                          "no factual content" = "#625D87")))
    
  })
  
  output$newplottwo <- renderPlot({
    new.data <- filtered()
    rating <- input$Rating
    category <- input$category
    
    if (category == "mainstream" && rating == "mostly false") {
      return()
    } else {
      return(
        ggplot(data = new.data, aes(x = Page, fill = Rating)) +
          geom_bar(stat = "count", position = position_dodge()) +
          theme(axis.text = element_text(size = 8)) +
          scale_fill_manual(values = c("mixture of true and false" = "#328BD3", 
                                       "mostly false" = "#0B5563", 
                                       "mostly true" = "#A2BCE0",
                                       "no factual content" = "#625D87"))
      )
    }
    
  })
  
  output$plotone.conclusion <- renderText({
    table.category <- table.category()
    return(
      paste(
        "The category with the most truthful information was mainstream, with ",
        table.category['Percentage of mainstream articles', 'mostly true'],
        " of truthful content.
        Mainstream content also had ",
        table.category['Percentage of mainstream articles', 'mostly false'],
        "of total false content and ",
        table.category['Percentage of mainstream articles', 'mixture of true and false'],
        "of a mixture of true and false content. The category in the middle was the left with",
        table.category['Percentage of left articles', 'mostly true'],
        " of truth content. Left content also had ",
        table.category['Percentage of left articles', 'mostly false'],
        "of total false content and",
        table.category['Percentage of left articles', 'mixture of true and false'],
        "of a mixture of true and false content. The category with the most false information is the right,
        with ",
        table.category['Percentage of right articles', 'mostly false'],
        " false content. Right content also had",
        table.category['Percentage of right articles', 'mostly true'],
        "of total truthful content and ",
        table.category['Percentage of right articles', 'mixture of true and false'],
        "of a mixture of true and false content. This data shows that users should try and aim to look at
        mainstream media on Facebook, as this data has proven that mainstream news 
        includes the most true information from all the different news categories."
      )
      )
    
    
  })
  
  output$plottwo.conclusion <- renderText({
    table.page <- table.page()
    
    return (
      paste(
        "The top three pages that are the most truthful are Politico (",
        table.page['Politico', 'mostly true'],
        "), CNN Politics (",
        table.page['CNN Politics', 'mostly true'],
        "), and ABC News Politics (",
        table.page['ABC News Politics', 'mostly true'],
        "). These all belong to the mainstream news category. The top three pages with the 
        most false content are Right Wing News (",
        table.page['Right Wing News', 'mostly false'],
        "), Freedom Daily (",
        table.page['Freedom Daily', 'mostly false'],
        "), and Eagle Rising (",
        table.page['Eagle Rising', 'mostly false'],
        "). These all belong to the right news category. From this information we can 
        further conclude that Facebook users should
        focus mostly on mainstream news providers, and avoid right and even left 
        categorical news as this information is mostly false or
        can contain false information."
        
      )
    )
    
  })

  ############## QUESTION 3 CODE #########################
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