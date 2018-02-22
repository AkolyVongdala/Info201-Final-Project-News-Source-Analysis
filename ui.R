source("server.R")

shinyUI(fluidPage(
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
                sidebarPanel("Side"),
                mainPanel("Main")                
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
