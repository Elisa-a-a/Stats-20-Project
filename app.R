library(shiny)


# -----------------------------------------------------------------------------
# 1. DATA
# -----------------------------------------------------------------------------

spots <- data.frame(
  id = 1:4,
  name = c("Powell Library", "Young Research Library",
           "Kerckhoff Hall", "Court of Sciences Student Center"),
  area = c("Central Campus", "North Campus", "Central Campus", "South Campus"),
  type = c("Library", "Library", "Student Union", "Student Center")
)

# Sample reviews (swap for real ones whenever you want)
reviews <- data.frame(
  spot_id = c(1, 1, 1, 1,   2, 2, 2,   3, 3, 3,   4, 4, 4),
  author  = paste0("Bruin_", 1:13),
  stars   = c(5, 5, 4, 5,   5, 4, 5,   4, 3, 5,   4, 3, 4),
  date    = c("Nov 2, 2026", "Oct 21, 2026", "Oct 5, 2026", "Sep 18, 2026",
              "Nov 10, 2026", "Oct 28, 2026", "Oct 1, 2026",
              "Nov 5, 2026", "Oct 15, 2026", "Sep 25, 2026",
              "Nov 12, 2026", "Oct 19, 2026", "Oct 2, 2026"),
  text    = c("Beautiful building and a quiet atmosphere. Great for midterms.",
              "Easy to focus here, but it gets crowded at night.",
              "Love the reading rooms. Outlets are limited so come early.",
              "Historic and inspiring. The study areas are comfortable.",
              "Lots of floors so you can always find a quiet corner.",
              "Great for long study sessions and the wifi is solid.",
              "My go-to spot for finals. Bring a sweater!",
              "Good for group work and coffee, but loud at lunch.",
              "Okay for a quick session but hard to find seats at noon.",
              "Cozy and central. Nice background energy.",
              "Spacious and rarely full. Good when the big libraries are packed.",
              "Decent seating but a bit far from the main walkways.",
              "Quiet in the mornings with plenty of tables.")
)

# NEW: if we already saved reviews to a file in an earlier run, load those
# instead of the sample ones. (write.csv happens when someone submits a review.)
if (file.exists("reviews.csv")) {
  reviews <- read.csv("reviews.csv", stringsAsFactors = FALSE)
}


# -----------------------------------------------------------------------------
# 2. FUNCTIONS
# -----------------------------------------------------------------------------

# NEW: this function now takes the reviews data frame as an input,
# because the reviews can change when someone submits a new one.
avg_rating <- function(id, data) {
  round(mean(data$stars[data$spot_id == id]), 1)
}

# Turn a number of stars into text, like 4 -> "★★★★☆"
star_text <- function(n) {
  paste0(strrep("\u2605", n), strrep("\u2606", 5 - n))
}


# -----------------------------------------------------------------------------
# 3. STYLING
# -----------------------------------------------------------------------------
app_css <- "
  body { background: #F4F2ED; font-family: Helvetica, Arial, sans-serif; }
  .side { width: 400px; height: 100vh; padding: 20px; overflow-y: auto; }

  .spot-card { display: flex; gap: 16px; background: white; border: 1px solid #E6E3DA;
               border-radius: 18px; padding: 14px; margin-bottom: 12px; cursor: pointer; }
  .spot-card:hover { border-color: #2F6B57; }
  .photo { width: 110px; height: 110px; background: #D9D6CA; border-radius: 14px;
           display: flex; align-items: center; justify-content: center; flex-shrink: 0; }
  .info { flex: 1; display: flex; flex-direction: column; justify-content: space-between; }
  .name { font-weight: bold; font-size: 17px; }
  .meta { color: #777; font-size: 14px; }
  .bottom { display: flex; justify-content: space-between; align-items: center; }
  .green-btn { background: white; color: #2F6B57; border: 1.5px solid #2F6B57;
               border-radius: 12px; padding: 8px 14px; font-weight: bold; }
  .solid-btn { background: #2F6B57; color: white; border: none;
               border-radius: 12px; padding: 10px 18px; font-weight: bold; }

  .bar-row { display: flex; align-items: center; gap: 8px; margin: 4px 0; }
  .bar-track { flex: 1; height: 10px; background: #E6E3DA; border-radius: 6px; }
  .bar-fill { height: 10px; background: #F4C242; border-radius: 6px; }
  .big-number { font-size: 48px; text-align: center; }
  .stars { color: #F4C242; }
  .review { padding: 12px 0; border-bottom: 1px solid #E6E3DA; }
"


# -----------------------------------------------------------------------------
# 4. UI
# -----------------------------------------------------------------------------
#puts the sidebar onto the screen
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  div(class = "side", uiOutput("sidebar"))
)


# -----------------------------------------------------------------------------
# 5. SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # NEW: the reviews now live in a reactiveVal so the app updates when we add one.
  # To READ it, write all_reviews().  To CHANGE it, write all_reviews(new_data).
  all_reviews <- reactiveVal(reviews)
  
  selected_spot <- reactiveVal(0)   # 0 = not looking at a spot's reviews
  rating_spot   <- reactiveVal(0)   # NEW: 0 = not on the rating form
  
  # ---- Button clicks ----
  
  # Card clicked -> show that spot's reviews
  observeEvent(input$open_spot, {
    selected_spot(input$open_spot)
  })
  
  # Back arrow on the reviews page -> back to the list
  observeEvent(input$back, {
    selected_spot(0)
  })
  
  # NEW: "Rate this spot" button on a card -> open the form for that spot
  observeEvent(input$rate_spot, {
    rating_spot(input$rate_spot)
  })
  
  # NEW: "Write a review" button on the reviews page -> open the form
  observeEvent(input$write_review, {
    rating_spot(selected_spot())
  })
  
  # NEW: Cancel button on the form -> close the form
  observeEvent(input$cancel_rating, {
    rating_spot(0)
  })
  
  # NEW: Submit button on the form -> save the review
  observeEvent(input$submit_review, {
    
    if (input$review_text == "") {
      # Don't allow an empty review
      showNotification("Please write a short review first.", type = "error")
      
    } else {
      # If they leave the name blank, call them Anonymous
      if (input$review_name == "") {
        who <- "Anonymous Bruin"
      } else {
        who <- input$review_name
      }
      
      # Make a one-row data frame with the same columns as `reviews`
      new_row <- data.frame(
        spot_id = rating_spot(),
        author  = who,
        stars   = as.numeric(input$review_stars),   # radio buttons give text, so convert
        date    = format(Sys.Date(), "%b %d, %Y"),
        text    = input$review_text
      )
      
      # Put the new review on TOP (rbind stacks data frames) and save the update
      updated <- rbind(new_row, all_reviews())
      all_reviews(updated)
      
      # Save to a file so the review is still there next time the app runs
      write.csv(updated, "reviews.csv", row.names = FALSE)
      
      # Show the spot's reviews page so they can see their new review
      selected_spot(rating_spot())
      rating_spot(0)
      showNotification("Thanks for your review!")
    }
  })
  
  
  # ---- The sidebar: rating form OR reviews OR list ----
  output$sidebar <- renderUI({
    
    if (rating_spot() != 0) {
      
      # ===== NEW: RATING FORM =====
      spot <- spots[spots$id == rating_spot(), ]
      
      tagList( #keilah code go here
        h3("Rate ", spot$name),
        p(class = "meta", paste(spot$area, "-", spot$type)),
        
        radioButtons("review_stars", "Your rating (5 is best)",
                     choices = 5:1, selected = 5, inline = TRUE),
        textInput("review_name", "Your name (optional)", width = "100%"),
        textAreaInput("review_text", "Your review", rows = 4, width = "100%",
                      placeholder = "How was the noise level, seating, outlets, wifi...?"),
        
        actionButton("submit_review", "Submit review", class = "solid-btn"),
        actionButton("cancel_rating", "Cancel", class = "green-btn")
      ) #keilah code end here
      
    } else if (selected_spot() == 0) {
      
      # ===== LIST VIEW =====
      tagList(
        h5("Search UCLA study spots"),
        textInput("search", label = NULL, placeholder = "Search by name or area of campus"),
        uiOutput("spot_list")
      )
      
    } else {
      
      # ===== REVIEWS VIEW =====
      data <- all_reviews()   # NEW: use the current reviews
      spot <- spots[spots$id == selected_spot(), ]
      these_reviews <- data[data$spot_id == selected_spot(), ]
      n <- nrow(these_reviews)
      avg <- avg_rating(selected_spot(), data)
      
      bars <- lapply(5:1, function(k) {
        count <- sum(these_reviews$stars == k)
        percent <- 100 * count / n
        div(class = "bar-row",
            span(k),
            div(class = "bar-track",
                div(class = "bar-fill", style = paste0("width:", percent, "%;"))))
      })
      
      review_list <- lapply(1:n, function(i) {
        r <- these_reviews[i, ]
        div(class = "review",
            strong(r$author), span(" - ", r$date),
            div(class = "stars", star_text(r$stars)),
            p(r$text))
      })
      
      tagList(
        actionButton("back", "\u2190 Back"),
        h3(spot$name),
        p(class = "meta", paste(spot$area, "-", spot$type)),
        
        fluidRow(
          column(7, bars),
          column(5,
                 div(class = "big-number", format(avg, nsmall = 1)),
                 div(class = "stars", style = "text-align:center;", star_text(round(avg))),
                 div(class = "meta", style = "text-align:center;", paste(n, "reviews")))
        ),
        
        br(),
        actionButton("write_review", "Write a review", class = "green-btn"),
        hr(),
        review_list
      )
    }
  })
  
  
  # ---- The list of spot cards ----
  output$spot_list <- renderUI({
    
    data <- all_reviews()   # NEW: so the ratings on the cards update too
    
    q <- tolower(input$search)
    if (is.null(q) || q == "") {
      shown <- spots
    } else {
      shown <- spots[grepl(q, tolower(spots$name)) | grepl(q, tolower(spots$area)), ]
    }
    
    if (nrow(shown) == 0) {
      return(p("No study spots found."))
    }
    
    lapply(1:nrow(shown), function(i) {
      s <- shown[i, ]
      
      # Copy-paste JavaScript that tells Shiny which spot was clicked.
      # stopPropagation() keeps the button click from also opening the card.
      open_js <- paste0("Shiny.setInputValue('open_spot', ", s$id, ", {priority: 'event'})")
      rate_js <- paste0("event.stopPropagation(); Shiny.setInputValue('rate_spot', ", s$id, ", {priority: 'event'})")
      
      div(class = "spot-card", onclick = open_js,
          div(class = "photo", "Photo"),
          div(class = "info",
              div(
                div(class = "name", s$name),
                div(class = "meta", paste(s$area, "-", s$type))
              ),
              div(class = "bottom",
                  strong(paste("\u2605", format(avg_rating(s$id, data), nsmall = 1))),
                  tags$button(class = "green-btn", onclick = rate_js, "Rate this spot"))
          ))
    })
  })
}

#hi
# -----------------------------------------------------------------------------
# 6. RUN
# -----------------------------------------------------------------------------
shinyApp(ui, server)