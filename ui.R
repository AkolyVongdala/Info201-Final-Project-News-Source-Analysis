library("shiny")

my.ui <- fluidPage(
  titlePanel("Country Carbon Emission Values"),
  sidebarLayout(
    # layout the page in two columns
    sidebarPanel(
      # specify content for the "sidebar" column
      # dropdown menu for year selection
      textOutput("In the first section, we look into the truth ratings of articles based off different news categories (Mainstream, Left, or Right.) With this data, we can see whether hyper-partisan effects the truthfulness of the articles posted. This can inform individuals on whether they should continue or be wary of news sources that fall under these categories, especially when browsing news on Facebook. 

                 In the second section, individuals can explore truth ratings of articles from different news sources (three from each category above.) This allows individuals to see which sources from different (or all) categories have the most trustworthy information, so they can continue to follow those and move away from content that is mostly false (or even a mixture of try and false.) "),
      
      selectInput(
        "category",
        "News Categories:",
        c(
          'All' = 'all',
          'Mainstream' = 'mainstream',
          'Left' = 'left',
          'Right' = 'right'
        ),
        selected = 'All'
      ),
      
      # radio buttons for data type selection
      # (kt vs. metric tons per capita)
      radioButtons(
        "Rating",
        "Truth Ratings",
        c(
          "All" = "all",
          "No Factual Content" = "no factual content",
          "Mostly True" = "mostly true",
          "Mixture of true and false" = "mixture of true and false",
          "Mostly False" = "mostly false"
        
        ),
        selected = 'all'
      )
    ),
   
    
    # displays outputs created from the server
    # aka the table and the map data, as well as their
    # descriptions
    mainPanel(
      tableOutput('category.table'),
      plotOutput('newplot'),
      tableOutput('page.table'),
      plotOutput('newplottwo')
  )
    
  )
)
