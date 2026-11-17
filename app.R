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

library(shiny)
library(DT)

reviews <- data.frame(
  reviewer_name = character(),
  date_stamp    = character(),
  stars_wifi    = numeric(),
  stars_etc     = numeric(),
  write_up      = character(),
  pictures      = I(list())
)

ui <- fluidPage(
  titlePanel("Reviews"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Reviewer name"),
      sliderInput("wifi", "Wifi rating", min = 1, max = 5, value = 3),
      sliderInput("outlet", "Outlet access", min = 1, max = 5, value = 3),
      sliderInput("traffic", "Traffic level", min = 1, max = 5, value = 3),
      sliderInput("noise", "Noise level", min = 1, max = 5, value = 3),
      sliderInput("seating", "Seating Availability", min = 1, max = 5, value = 3),
      textAreaInput("text", "Overall Thoughts"),
      fileInput("pics", "Pictures", multiple = TRUE, accept = "image/*"),
      actionButton("add", "Add review", class = "btn-primary")
    ),
    mainPanel(DTOutput("tbl"))
  )
)

server <- function(input, output, session) {
  rv <- reactiveVal(reviews)
  
  # "Add review" button -> append a row to the data frame
  observeEvent(input$add, {
    req(input$name)
    new <- data.frame(
      reviewer_name = input$name,
      date_stamp    = format(Sys.Date()),
      stars_wifi    = input$wifi,
      stars_etc     = input$etc,
      write_up      = input$text
    )
    new$pictures <- I(list(if (is.null(input$pics)) character() else input$pics$datapath))
    rv(rbind(rv(), new))
  })
  
  # Render the table with a Delete button on every row
  output$tbl <- renderDT({
    df <- rv()
    req(nrow(df) > 0)
    
    df$pictures <- vapply(df$pictures, length, integer(1))  # show picture count
    df$actions <- sprintf(
      '<button class="btn btn-sm btn-danger"
         onclick="Shiny.setInputValue(\'delete_row\', %d, {priority: \'event\'})">Delete</button>',
      seq_len(nrow(df))
    )
    
    datatable(df, escape = FALSE, selection = "none", rownames = FALSE)
  })
  
  # Delete button clicked -> drop that row
  observeEvent(input$delete_row, {
    rv(rv()[-input$delete_row, , drop = FALSE])
  })
}

shinyApp(ui, server)

#Request