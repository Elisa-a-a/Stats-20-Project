setwd("~/Documents/GitHub")
getwd()

library(shiny)
library(bslib)


#Initial Loading Page (Map + Sidebar)

#SideBar
ui <- page_sidebar(
  title = "StudySpots",
  theme = bs_theme(version = 5, primary = "#1F6F5C", bg = "#F6F4EF", fg = "#1F2421"))

  sidebar = sidebar(
    width = 380,
    textInput("q", "Search study spots",
              placeholder = "Search by building name or location", width = "100%"),
    uiOutput("cards"))
  





#Review



#Request



shinyApp(ui = ui, server = server)
