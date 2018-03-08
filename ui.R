library(shinydashboard)
library(shiny)
source("server.R")

shinyUI(dashboardPage(skin = "black",
  dashboardHeader(title = "News Source Analysis"),
  # Sidebar tabs
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("home")),
    menuItem("Factualness and Popularity", tabName = "tab1", icon = icon("file")),
    menuItem("Mainstream vs Hyperpartisan", tabName = "tab2", icon = icon("file")),
    menuItem("Factualness and Media Types", tabName = "tab3", icon = icon("file"))
  )),
  # Content of each page
  dashboardBody(tabItems(
    # Content of "About" Page
    tabItem(tabName = "about", title = "About", 
            # Fun and eye catching header
            fluidRow(column(width = 12, align = "center", 
                            box(width = 12, h1(em("Is this fake news?!?")), background = "navy"))),
            # Information about the data
            fluidRow(box(title = "The Data", status = "primary",
                         p(a(href = "https://github.com/BuzzFeedNews/2016-10-facebook-fact-check/blob/master/data/facebook-fact-check.csv", "The dataset"), " report looks into BuzzfeedNews data that contains fact-checked reviews of more than 1,000 Facebook posts taken from Facebook pages. This data takes posts made by nine main news source pages on Facebook, each of them falling into news categories of Mainstream, Left, or Right.  These posts were made during the time of the 2016 presidential election. The data also contains information on the post type (video, link, text, or photo) and the number of shares/comments/reacts these posts got. With this report, we were able to analyze and make conclusions in topics related to factualness vs. popularity, factualness vs. news categories, and factualness vs. media type."),
                         br(),
                         img(src = "facebook-photo.JPG", width = "100%"),
                         br(), br(),
                         strong("Guide in which each post was reviewed:"),
                         p(em("Mostly True:"), " The post and any related link or image are based on factual 
                           information and portray it accurately. Did not misrepresent events, numbers, quotes, 
                           reactions, etc., or make information up."),
                         p(em("Mixture of True and False:")," Some elements of the information are factually 
                           accurate, but some elements or claims are not. This rating should be used when 
                           speculation or unfounded claims are mixed with real events, numbers, quotes, etc."),
                         p(em("Mostly False:"), " Most or all of the information in the post or in the link 
                           being shared is inaccurate. This should also be used when the central claim being 
                           made is false."),
                         p(em("No Factual Content:"), " This rating is used for posts that are pure opinion, 
                           comics, satire, or any other posts that do not make a factual claim. This is also 
                           the category to use for posts that are of the \"Like this if you think...\" variety."),
                         strong("More Definitions:"),
                         p(em("Popularity types:"), " There are three different categories which popularity 
                           is broken into: share counts, reaction counts and comment counts. Each of these 
                           different counts is used to asses popularity levels of certain posts."),
                         p(em("Mainstream:"), " News sources that don't openly affiliate with a political party."),
                         p(em("Hyperpartisan:"), " News sources that openly affiliate with a political party 
                           and articles relate heavily to what that political party stands for."),
                         p(em("Media types:"), " The posts fall into four different media categories: text, link,
                           video, photo.")),
                     # The questions we are asking and their corresponding tabs
                     box(title = "The Questions", status = "warning",
                         p(strong("Factualness and Popularity: "), "How does the factualness of a post correlate 
                           with its popularity?"),
                         p(strong("Mainstream verus Hyperpartisan: "), "Do \"mainstream\" news sources post more 
                           factual articles than hyperpartisan news sources?"),
                         p(strong("Factualness and Media Types: "), "Are certain media types of posts less factual?")),
                     # The goals of this report
                     box(title = "Goals", status = "warning", 
                         p("The goal for this data report is to help give users the tools and information they 
                           need to navigate Facebook's news feed so they can receive news from sources that are 
                           truthful and sincere. Anyone can publish to Facebook, and the information does not 
                           necessarily have to be fact-checked, so it is very easy for false information to float 
                           around Facebook News Feeds. We will be looking into different news categories, news pages, 
                           media types, and post engagement.")),
                     # Target Audience
                     box(title = "User Group", status = "warning",
                         p("Our user group that we are aiming for are typical Facebook users, especially 
                           those who use Facebook as a news source in their daily lives. As the", 
                           a("age of misinformation", 
                             href = "https://www.npr.org/sections/thetwo-way/2016/11/19/502692075/misinformation-on-facebook-zuckerberg-lists-ways-of-fighting-fake-news"), 
                           "is upon us, many users are still unaware of the amount of false information that is distributed 
                           on Facebook. We hope to arrive at conclusions that can help these users navigate Facebook to 
                           ensure they have the tools to understand and seek out news that is truthful and sincere.")))),
    # Content of Q1 page
    tabItem(tabName = "tab1",
            # Main question
            fluidRow(column(width = 12, align = "center", 
                            box(width = 12, h2("How does the factualness of a post correlate with its popularity?"), background = "navy"))),
            fluidRow(
              # Main content of page with graphs and conclusion
              column(width = 9,
                box(width = 12, status = "primary",
                    h3("Popularity Types & Factualness by Counts"), plotOutput("popularity.plot"), 
                    textOutput("popularity.text")),
                box(width = 12, status = "primary",
                    h3("Popularity Types & Factualness by Counts per Post"), plotOutput("popularity.percents"),
                    textOutput("popularity.per.text")),
                box(width = 12, status = "primary",
                  h3("Individual Plots for Popularity Types & Factualness"), plotOutput("popularity.grid"),
                  textOutput("popularity.grid.text")),
                box(title = "Conclusion", status = "primary", width = 12,
                    p("Based off the data and visualizations, it is very clear that posts with \"no 
                      factual content\" are the most popular. That is not surprising because posts 
                      on Facebook containing no factual content tend to be for entertainment, which 
                      is a huge portion of what people use Facebook for. Posts that are the least 
                      popular are the one that are \"mostly true\", this conclusion is drawn from 
                      the second graph which shows the counts per post. Since the data doesn't include 
                      information about the comments or reactions, no solid conclusions can be drawn 
                      about the reason behind this. However, we speculated that people are less likely 
                      to comment on, react to or share something with a strong position than something 
                      that is more agreeable."),
                    p("There is also an interesting trend between all the rating categories versus the 
                      \"mostly false\" one. In every rating category, except for \"mostly false\" the 
                      share count is higher than the other popularity counts. In the \"mostly false\" 
                      category the reaction count has the highest numbers. We believe that this is because 
                      when people share a post, they are saying to all their friends that they completely 
                      agree with it. People share the nonfactual posts a lot more because its easy to agree 
                      with something that isn't political. But when it comes to the mostly false posts, 
                      people share it less than they react to it, because they are more skeptical of its 
                      content and not willing to share with everyone they fully believe it. That being said, 
                      this speculation is based off of the data, but also our own experience with people and 
                      people using Facebook."))
                ),
              # Side bar containing significance of the question and controls
              box(width = 3, status = "warning", title = "Significance", 
                  p("This question is significant because if there is a correlation between factualness and 
                    popularity, the relationship would reveal what people care about when finding their news. 
                    Popular news gets shared across many platforms, is talked about more, and becomes more 
                    influential. It is important to be conscientious of what news becomes influential.")),
              box(title = "Controls", width = 3, status = "warning",
                  checkboxGroupInput("popularity.types", label = "Select popularity types to display:", 
                                     choices = c("Comments" = "comment_count",
                                                 "Reactions" = "reaction_count",
                                                 "Shares" = "share_count")))
            )),
    # Content of Q2 page
    tabItem(tabName = "tab2", 
            # The qustion
            fluidRow(column(width = 12, align = "center", 
                            box(width = 12, h3("Do \"mainstream\" news sources post more factual 
                                               articles than hyperpartisan news sources?"), background = "navy"))),
            # Main part of page, contains graphs, tables and a conclusion
            fluidRow(column(width = 9, status = "primary", 
                            box(width = 12, h3("Categories Vs. Truth Rating"), status = "primary",
                                p("In the first section, we look into the truth ratings of articles 
                                  based off different news categories (Mainstream, Left, or Right.) 
                                  With this data, we can see whether hyper-partisan effects the truthfulness 
                                  of the articles posted. This can inform individuals on whether 
                                  they should continue or be wary of news sources that fall under these 
                                  categories, especially when browsing news on Facebook. "),
                                tableOutput("category.table"),
                                textOutput('plotone.conclusion'),
                                plotOutput('newplot')),
                            box(width = 12, status = "primary", h3("Pages Vs. Truth Rating"),
                                p("In the second section, individuals can explore truth ratings of articles 
                                  from different news sources (three from each category above.) 
                                  This allows individuals to see which sources from different (or all) 
                                  categories have the most trustworthy information, so they can continue 
                                  to follow those and move away from content that is mostly false (or even 
                                  a mixture of try and false.) "),
                                tableOutput('page.table'),
                                textOutput('plottwo.conclusion'),
                                plotOutput('newplottwo')),
                            box(width = 12, title = "Conclusion", status = "primary",
                                p("Overall, when inspecting these news source categories of these different 
                                  Facebook new pages, we can conclude that news sources that take a left/right 
                                  side tend to contain more false information than mainstream news sources. 
                                  The significance of this finding is important as Facebook's current 
                                  algorithm tends to only show its user's content that they typically 
                                  engage in. So if users tend to engage only with news pages that have 
                                  these extreme political sides, they may be losing access to more 
                                  truthful information and gaining access to more false information. 
                                  We hope these results can help users be more conscious of the news 
                                  they interact with on their Facebook news feed. This concept is known 
                                  as the filter bubble, which you can learn more about here:", 
                                  a("https://www.ted.com/talks/eli_pariser_beware_online_filter_bubbles")))),
                      # Side bar, contains significance of question and controls
                      box(width = 3, status = "warning", title = "Significance",
                         p("It is important to determine if \"mainstream\" news sources are more factual 
                           than hyperpartisan news sources because people generally follow news sources 
                           with similar political stances as their own. We want to provide information 
                           to people who seek news through Facebook so that they will be able to make 
                           more educated decisions when it comes to what sources to trust.")),
                     box(title = "Controls", width = 3, status = "warning",
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
                         )))),
    # Content of Q3 Page
    tabItem(tabName = "tab3",
            # Question
            fluidRow(column(width = 12, align = "center", 
                            box(width = 12, h2("Are certain media types of posts less factual?"), background = "navy"))),
            fluidRow(column(width = 9,
                            # Main part of page, contains graphh and conclusion
                            box(width = 12, plotOutput("media.factualness"), status = "primary",
                                h5("Graph description"),
                                textOutput("plot.text")),
                            box(width = 12, title = "Conclusion", status = "primary",
                                p("From the trend analysis of the types of media (texts, photos, links and 
                                  videos) and its factualness rating on different facebook posts, we can 
                                  see how each media type and its rating correlates to how different news 
                                  sources use these media types to share different factual information. 
                                  This trend advises users on how each news sources use different media 
                                  types to their advantages. For example, through this data you can see 
                                  that links contribute mostly towards the factualness rating of 'Mostly 
                                  True'. This is significance because from understanding how each media 
                                  types can hold different factualness rating will help the users to 
                                  managed their engagement with different types of medias across different 
                                  news sources."))),
                     # Sidebar containing significance and controls
                     box(width = 3, title = "Significance", status = "warning",
                         p("It is necessary to know if certain media types of posts tend to be less factual 
                           because this information can help people make informed decisions about what news to trust. 
                           While this may seem like a small discovery, knowing this would help keep readers more aware when 
                           reading news posts. We will also analyze which media types are shared more and how 
                           that relates to its factualness.")),
                     box(width = 3, title = "Controls", status = "warning",
                         radioButtons("radio.factualness", label = h4("Select the type of media:"), 
                                      choices = list("Video" =  'video', "Link" = 'link', "Photo" = 'photo', "Text" = 'text')),
                         h4("Trend Summary:"),
                         textOutput("media.text"))))
  ))
))



