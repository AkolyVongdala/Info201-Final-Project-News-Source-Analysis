library("shiny")
library("dplyr")
library("ggplot2")
library("maps")


# server that takes in information from the UI
my.server <- function(input, output) {
  # gets the data from the data folder
  data <-
    read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)
  
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
             geom_bar(stat = "count", position = position_dodge()))
    
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
          geom_bar(stat = "count", position = position_dodge())
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
        mainstream media on Facebook, as this data has proven that mainstream news includes the most true information from all the different news categories."
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
      "). These all belong to the mainstream news category. The top three pages with the most false content are Right Wing News (",
      table.page['Right Wing News', 'mostly false'],
      "), Freedom Daily (",
      table.page['Freedom Daily', 'mostly false'],
      "), and Eagle Rising (",
      table.page['Eagle Rising', 'mostly false'],
      "). These all belong to the right news category. From this information we can further conclude that Facebook users should
      focus mostly on mainstream news providers, and avoid right and even left categorical news as this information is mostly false or
      can contain false information."
      
    )
  )
  
  
  
  })

}