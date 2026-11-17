setwd("~/Documents/GitHub")
getwd()

library(shiny)
library(bslib)

#SideBar
ui <- page_sidebar(
  # App title ----
  title = "Study Spots!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of Bins:",
      min = 2,
      max = 50,#hi
      value = 30
    )
  ),
  # Output: Histogram ----
 
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui = ui, server = server)


#Initial Loading Page (Map + Sidebar)


#Review


#Request