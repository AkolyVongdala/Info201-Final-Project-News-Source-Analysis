library("shiny")
library("dplyr")
library("ggplot2")
library("maps")


# server that takes in information from the UI
my.server <- function(input, output) {
  # creates a table that is filtered from user input (the widgets)
  data <- read.csv("data/facebook-fact-check.csv", stringsAsFactors = FALSE)
  filtered <- reactive({
      rating <- input$Rating 
      category <- input$category;
      
      
      if(category != "all"){
        data <- data %>% filter(Category == category) 
      } 
      if(rating != "all") {
        data <- data %>% filter(Rating == rating) %>% select(Category, Rating, Page);
      
      } 
        return(data);
      
    
  })
  
  
  
  output$category.table <- renderTable( rownames = TRUE, {
    rating <- input$Rating 
    category <- input$category;
    
    table.making <- function(category){
      data <- data %>% filter(Category == category);
      total <- nrow(data);
      total.percentage <- '100%'
      
      if(rating != "all") {
        no.factual.content <- nrow(data %>% filter(Rating == rating));
        rating.percentage <- paste(round((no.factual.content / total) * 100), "%", sep="")
        all.percentages <-data.frame(rating.percentage,stringsAsFactors = FALSE )
        colnames(all.percentages) <- c(rating)
        
        return(all.percentages)
        
      } else {
        no.factual.content <- nrow(data %>% filter(Rating == 'no factual content'));
        nfc.percentage <- paste(round((no.factual.content / total) * 100), "%", sep="")
      
        mostly.true <- nrow(data %>% filter(Rating == 'mostly true'));
        true.percentage <- paste(round((mostly.true / total) * 100), "%", sep="")
      
        mixture.tandf <- nrow(data %>% filter(Rating == 'mixture of true and false'));
        tandf.percentage <- paste(round((mixture.tandf / total) * 100), "%", sep="")
      
        mostly.false <- nrow(data %>% filter(Rating == 'mostly false'));
        false.percentage <- paste(round((mostly.false / total) * 100), "%", sep="")
      
        all.percentages <-data.frame(tandf.percentage, false.percentage, true.percentage, nfc.percentage, total.percentage,stringsAsFactors = FALSE )
      
        colnames(all.percentages) <- c("Mixture of True and False", "Mostly False", "Mostly True", 'No Factual Content', "Total")
  
        return(all.percentages)
      }
      
      
      
    }
    
    if(category != "all"){
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
      rows <- c('Percentage of mainstream articles', 'Percentage of left articles', 'Percentage of right articles')
      
      row.names(table.one) <- rows
      return(table.one)
      
      
    } 
    if(rating != "all") {
      data <- data %>% filter(Rating == rating) %>% select(Category, Rating, Page);
      
    } 
    return(data);
    
  })
  
  
  
  output$page.table <- renderTable( rownames = TRUE,{
  rating <- input$Rating 
  category <- input$category;
  
  table.making <- function(page){
    data <- data %>% filter(Page == page);
    total <- nrow(data);
    total.percentage <- '100%'
    
    if(rating != "all") {
      no.factual.content <- nrow(data %>% filter(Rating == rating));
      rating.percentage <- paste(round((no.factual.content / total) * 100), "%", sep="")
      all.percentages <-data.frame(rating.percentage,stringsAsFactors = FALSE )
      colnames(all.percentages) <- c(rating)
      
      return(all.percentages)
      
    } else {
      no.factual.content <- nrow(data %>% filter(Rating == 'no factual content'));
      nfc.percentage <- paste(round((no.factual.content / total) * 100), "%", sep="")
      
      mostly.true <- nrow(data %>% filter(Rating == 'mostly true'));
      true.percentage <- paste(round((mostly.true / total) * 100), "%", sep="")
      
      mixture.tandf <- nrow(data %>% filter(Rating == 'mixture of true and false'));
      tandf.percentage <- paste(round((mixture.tandf / total) * 100), "%", sep="")
      
      mostly.false <- nrow(data %>% filter(Rating == 'mostly false'));
      false.percentage <- paste(round((mostly.false / total) * 100), "%", sep="")
      
      all.percentages <-data.frame(tandf.percentage, false.percentage, true.percentage, nfc.percentage, total.percentage,stringsAsFactors = FALSE )
      
      colnames(all.percentages) <- c("Mixture of True and False", "Mostly False", "Mostly True", 'No Factual Content', "Total")
      
      return(all.percentages)
    }
    
    
    
  }
  
  mainstream.page.one <- table.making("ABC News Politics")
  mainstream.page.two <- table.making("CNN Politics")
  mainstream.page.three <- table.making("Politico")
  
  mainstream.page.one[nrow(mainstream.page.one) + 1,] <- mainstream.page.two
  mainstream.page.one[nrow(mainstream.page.one) + 1,] <- mainstream.page.three
  
  mainstream.rows <- c("ABC News Politics", "CNN Politics", "Politico")
  row.names(mainstream.page.one) <- mainstream.rows

  left.page.one <- table.making("Addicting Info")
  left.page.two <- table.making("Occupy Democrats")
  left.page.three <- table.making("The Other 98%")
  
  left.page.one[nrow(left.page.one) + 1,] <- left.page.two
  left.page.one[nrow(left.page.one) + 1,] <- left.page.three
  
  left.rows <- c("Addicting Info", "Occupy Democrats", "The Other 98%")
  
  row.names(left.page.one) <- left.rows

  
  right.page.one <- table.making("Eagle Rising")
  right.page.two <- table.making("Freedom Daily")
  right.page.three <- table.making("Right Wing News")
  
  right.page.one[nrow(right.page.one) + 1,] <- right.page.two
  right.page.one[nrow(right.page.one) + 1,] <- right.page.three
  
  right.rows <- c("Eagle Rising", "Freedom Daily", "Right Wing News")
  row.names(right.page.one) <- right.rows

  
  #all.pages <- data.frame(mainstream.page.one, left.page.one, right.page.one);
  
  if(category == "mainstream"){
    return(mainstream.page.one)
  } else if(category == "left") {
    return(left.page.one)
  } else if(category == "right") {
    return(right.page.one)
  } else {
    all.pages <- mainstream.page.one
    all.pages <- full_join(all.pages, left.page.one)
    all.pages <- full_join(all.pages, right.page.one)
    all.rows <- c("ABC News Politics", "CNN Politics", "Politico", "Addicting Info", "Occupy Democrats", "The Other 98%", "Eagle Rising", "Freedom Daily", "Right Wing News")
    row.names(all.pages) <- all.rows
    
    return(all.pages)
    
  }
  if(rating != "all") {
    data <- data %>% filter(Rating == rating) %>% select(Category, Rating, Page);
    
  } 
  return(data);
    
  })
                                        

  
  output$newplot <- renderPlot({
    data <- filtered()
    
    return(
           ggplot(data=data, aes(x=Category,fill=Rating)) +
             geom_bar(stat="count", position=position_dodge()))
    
  })
  
  output$newplottwo <- renderPlot({
    new.data <- filtered()
    
    return(
      ggplot(data=new.data, aes(x=Page,fill=Rating)) +
        geom_bar(stat="count", position=position_dodge()))
    
  })
  
  

}