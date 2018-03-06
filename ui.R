library(shinydashboard)
library(shiny)
source("Server.r")

shinyUI(dashboardPage(skin = "black",
  dashboardHeader(title = "News Source Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("home")),
    menuItem("Factualness and Popularity", tabName = "tab1", icon = icon("file")),
    menuItem("Mainstream vs Hyperpartisan", tabName = "tab2", icon = icon("file")),
    menuItem("Factualness and Media Types", tabName = "tab3", icon = icon("file"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "about", title = "About", 
            fluidRow(box(title = "The Data", status = "primary",
                         p(a(href = "https://github.com/BuzzFeedNews/2016-10-facebook-fact-check/blob/master/data/facebook-fact-check.csv", "The dataset"), " we are considering for this project has reviews of more than 1,000 Facebook posts from six large hyperpartisan and mainstream Facebook pages during the 2016 presidential election. Other information this data set contains about these posts are: the type of media it was (video, photo, link, etc.), how many comments/shares/reactions the post got, and how trustworthy the article was (based off established fact-checkers Facebook hired.) With this dataset, we can analyze how Facebook users react to different types of news information and how Facebook could have caused misinformation among its users regarding information surrounding the election. "),
                         strong("Guide in which each post was reviewed:"),
                         p(em("Mostly True:"), " The post and any related link or image are based on factual information and portray it accurately. Did not misrepresent events, numbers, quotes, reactions, etc., or make information up."),
                         p(em("Mixture of True and False:")," Some elements of the information are factually accurate, but some elements or claims are not. This rating should be used when speculation or unfounded claims are mixed with real events, numbers, quotes, etc."),
                         p(em("Mostly False:"), " Most or all of the information in the post or in the link being shared is inaccurate. This should also be used when the central claim being made is false."),
                         p(em("No Factual Content:"), " This rating is used for posts that are pure opinion, comics, satire, or any other posts that do not make a factual claim. This is also the category to use for posts that are of the \"Like this if you think...\" variety."),
                         strong("More Definitions:"),
                         p(em("Popularity types:"), "There are three different categories which popularity is broken into: share counts, reaction counts and comment counts. Each of these different counts is used to asses popularity levels of cerain posts."),
                         p(em("Mainstream:")),
                         p(em("Hyperpartisan:")),
                         p(em("Media types:"), "The posts fall into five different media cateogies: text, link, video, photo ")),
                     box(title = "The Questions", status = "primary",
                         p(strong("Factualness and Popularity: "), "How does the factualness of a post correlate with its popularity?"),
                         p(strong("Mainstream verus Hyperpartisan: "), "Do \"mainstream\" news sources post more factual articles than hyperpartisan news sources?"),
                         p(strong("Factualness and Media Types: "), "Are certain media types of posts less factual?")),
                     box(title = "The Results", status = "warning"))),
    tabItem(tabName = "tab1",
            fluidRow(
              column(width = 9,
                box(width = 12,
                    h3("Popularity Types & Factualness by Counts"),plotOutput("popularity.plot"), 
                    textOutput("popularity.text")),
                box(width = 12,
                    h3("Popularity Types & Factualness by Counts per Post"), plotOutput("popularity.percents"),
                    textOutput("popularity.per.text")),
                box(width = 12,
                  h3("Individual Plots for Popularity Types & Factualness"), plotOutput("popularity.grid"))
              ),
              box(width = 3, solidHeader = TRUE,
                  h4(strong("How does the factualness of a post correlate with its popularity?")),
                  h3("Significance"),
                  p("This question is significant because if there is a correlation between 
                    factualness and popularity, the relationship would reveal what people care 
                    about when finding their news. Popular news gets shared across many 
                    platforms, is talked about more and becomes more influential. It is 
                    important to be conscientious of what news is allowed to become influential, 
                    that is why we need to analyze the factualness of posts with its popularity.")),
              box(title = "Controls", width = 3,  
                  checkboxGroupInput("popularity.types", label = "Select popularity types to display:", 
                                     choices = c("Comments" = "comment_count",
                                                 "Reactions" = "reaction_count",
                                                 "Shares" = "share_count")))
            )),
    tabItem(tabName = "tab2"),
    tabItem(tabName = "tab3")
  ))
))



(fluidPage(
  titlePanel("News Source Analysis"),
  tabsetPanel(type = "tabs",
              tabPanel("About", h2("Data Set Description: BuzzFeed News 2016 Facebook Fact Check"),
                       fluidRow(column(5, p("The dataset we are considering for this project has reviews of more than 1,000 Facebook posts from six large hyperpartisan and mainstream Facebook pages during the 2016 presidential election. Other information this data set contains about these posts are: the type of media it was (video, photo, link, etc.), how many comments/shares/reactions the post got, and how trustworthy the article was (based off established fact-checkers Facebook hired.) With this dataset, we can analyze how Facebook users react to different types of news information and how Facebook could have caused misinformation among its users regarding information surrounding the election. "),
                                       a(href = "https://github.com/BuzzFeedNews/2016-10-facebook-fact-check/blob/master/data/facebook-fact-check.csv", "This dataset"), "was provided by buzzfeed.",
                                       "It contains the data and analysis for the BuzzFeed News article, \"Hyperpartisan Facebook Pages Are Publishing False And Misleading Information At An Alarming Rate,\" published October 20, 2016. Although this article contains manipulations of the data into visualizations, we believe we can created more impressive visualizations to making conclusions from.",
                                       h3("Guide in which each post was reviewed:"),
                                       p(em("Mostly True:"), " The post and any related link or image are based on factual information and portray it accurately. Did not misrepresent events, numbers, quotes, reactions, etc., or make information up."),
                                       p(em("Mixture of True and False:")," Some elements of the information are factually accurate, but some elements or claims are not. This rating should be used when speculation or unfounded claims are mixed with real events, numbers, quotes, etc."),
                                       p(em("Mostly False:"), " Most or all of the information in the post or in the link being shared is inaccurate. This should also be used when the central claim being made is false."),
                                       p(em("No Factual Content:")), " This rating is used for posts that are pure opinion, comics, satire, or any other posts that do not make a factual claim. This is also the category to use for posts that are of the \"Like this if you think...\" variety."), 
                                         column(7, img(src = "facebook-photo.JPG", width = 700)))
                       ),
              tabPanel("Factualness and Popularity", sidebarLayout(
                sidebarPanel(                             
                             h3("Question"),
                             h4("How does the factualness of a post correlate with its popularity?"),
                             h3("Significance"),
                             p("This question is significant because if there is a correlation between 
                               factualness and popularity, the relationship would reveal what people care 
                               about when finding their news. Popular news gets shared across many 
                               platforms, is talked about more and becomes more influential. It is 
                               important to be conscientious of what news is allowed to become influential, 
                               that is why we need to analyze the factualness of posts with its popularity."),
                             h3("Controls"),
                             checkboxGroupInput("popularity.types", label = "Select popularity types to display:", 
                                                choices = c("Comments" = "comment_count",
                                                            "Reactions" = "reaction_count",
                                                            "Shares" = "share_count"))),
                mainPanel(h3("Popularity Types & Factualness by Counts"),plotOutput("popularity.plot"), 
                          textOutput("popularity.text"),
                          h3("Popularity Types & Factualness by Counts per Post"), plotOutput("popularity.percents"),
                          textOutput("popularity.per.text"),
                          h3("Individual Plots for Popularity Types & Factualness"), plotOutput("popularity.grid"))                
              )),
              tabPanel("Mainstream vs Hyperpartisan", sidebarLayout(
                sidebarPanel("Side"),
                mainPanel("Main")                
              )),
              tabPanel("Factualness and Media Types", sidebarLayout(
                sidebarPanel("Side"),
                mainPanel("Main")                
              ))
  )
))


