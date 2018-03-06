library("shiny")

source('ui.R')
source('Server.R')

shinyApp(ui = my.ui, server = my.server)


