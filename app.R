library(shiny)


#DATA

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

# If we already saved reviews to a file in an earlier run, load those instead.
if (file.exists("reviews.csv")) {
  reviews <- read.csv("reviews.csv", stringsAsFactors = FALSE)
}


# -----------------------------------------------------------------------------
# 2. FUNCTIONS
# -----------------------------------------------------------------------------

avg_rating <- function(id, data) {
  round(mean(data$stars[data$spot_id == id]), 1)
}

# Turn a number of stars into text, like 4 -> "★★★★☆"
star_text <- function(n) {
  paste0(strrep("\u2605", n), strrep("\u2606", 5 - n))
}

# ---- Send the full review (with category ratings) somewhere else ----
send_review_elsewhere <- function(review) {
  # Example: my_reviews <<- rbind(my_reviews, review)
  message("Detailed review submitted by: ", review$reviewer_name)  # placeholder
}

# A row of 5 clickable star buttons
star_input <- function(id, label) {
  tags$div(
    class = "star-group", `data-input` = id,
    tags$label(label),
    tags$div(
      lapply(1:5, function(i) {
        tags$button(type = "button", class = "star-btn",
                    `data-value` = i, HTML("&#9733;"))
      })
    )
  )
}

star_ids <- c("star_wifi", "star_outlet", "star_traffic", "star_noise", "star_seating")


# -----------------------------------------------------------------------------
# 3. STYLING
# -----------------------------------------------------------------------------
app_css <- "
  body { background: #F4F2ED; color: #222; font-family: Helvetica, Arial, sans-serif; }
  .side { width: 400px; max-width: 100%; height: 100vh; padding: 20px; overflow-y: auto; }

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

  /* Full-screen review page: covers the whole viewport on any device */
  .review-fullscreen { position: fixed; top: 0; left: 0; right: 0; bottom: 0;
                       z-index: 1000; background: #F4F2ED; color: #222;
                       overflow-y: auto; padding: 24px 40px; box-sizing: border-box; }
  .review-fullscreen .form-group { max-width: 700px; }
  .review-fullscreen textarea { min-height: 150px; }
  .star-group { margin-bottom: 12px; }
  .star-btn { background: none; border: none; font-size: 30px;
              color: #ccc; cursor: pointer; padding: 0 2px; }
  .star-btn.active, .star-btn:hover { color: #f5b301; }
  @media (max-width: 600px) { .review-fullscreen { padding: 16px; } }
"

star_js <- "
  // Clicking a star lights up all stars up to it and sends the value to Shiny
  $(document).on('click', '.star-btn', function() {
    var group = $(this).closest('.star-group');
    var val = $(this).data('value');
    group.find('.star-btn').each(function() {
      $(this).toggleClass('active', $(this).data('value') <= val);
    });
    Shiny.setInputValue(group.data('input'), val);
  });

  // Clear stored ratings each time the form opens
  Shiny.addCustomMessageHandler('reset_stars', function(ids) {
    ids.forEach(function(id) { Shiny.setInputValue(id, null); });
  });
"


# -----------------------------------------------------------------------------
# 4. UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(star_js))),
  div(class = "side", uiOutput("sidebar"))
)


# -----------------------------------------------------------------------------
# 5. SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  all_reviews   <- reactiveVal(reviews)
  selected_spot <- reactiveVal(0)   # 0 = not looking at a spot's reviews
  rating_spot   <- reactiveVal(0)   # 0 = not on the rating form
  
  # Open the full-screen form for a spot (and clear any old star ratings)
  open_form <- function(spot_id) {
    session$sendCustomMessage("reset_stars", as.list(star_ids))
    rating_spot(spot_id)
  }
  
  # ---- Button clicks ----
  observeEvent(input$open_spot, { selected_spot(input$open_spot) })
  observeEvent(input$back,      { selected_spot(0) })
  
  # "Rate this spot" on a card -> open the form
  observeEvent(input$rate_spot, { open_form(input$rate_spot) })
  
  # "Write a review" on the reviews page -> open the form
  observeEvent(input$write_review, { open_form(selected_spot()) })
  
  # Back button on the form -> all the way back to the landing page
  observeEvent(input$cancel_rating, {
    rating_spot(0)
    selected_spot(0)
  })
  
  # Submit on the form -> save the review
  observeEvent(input$submit_review, {
    
    ratings <- c(input$star_wifi, input$star_outlet, input$star_traffic,
                 input$star_noise, input$star_seating)
    
    if (length(ratings) < 5) {
      showNotification("Please rate all 5 categories.", type = "warning")
      
    } else if (trimws(input$review_text) == "") {
      showNotification("Please write a short review first.", type = "error")
      
    } else {
      who <- if (trimws(input$review_name) == "") "Anonymous Bruin" else input$review_name
      
      # Overall stars = average of the 5 categories, rounded to a whole star
      overall <- round(mean(as.numeric(ratings)))
      
      # Row for the app's own reviews (same columns as `reviews`)
      new_row <- data.frame(
        spot_id = rating_spot(),
        author  = who,
        stars   = overall,
        date    = format(Sys.Date(), "%b %d, %Y"),
        text    = input$review_text
      )
      
      updated <- rbind(new_row, all_reviews())
      all_reviews(updated)
      write.csv(updated, "reviews.csv", row.names = FALSE)
      
      # Full detail row, including each category rating
      send_review_elsewhere(data.frame(
        reviewer_name = who,
        date_stamp    = format(Sys.Date()),
        spot_id       = rating_spot(),
        stars_wifi    = as.numeric(input$star_wifi),
        stars_outlet  = as.numeric(input$star_outlet),
        stars_traffic = as.numeric(input$star_traffic),
        stars_noise   = as.numeric(input$star_noise),
        stars_seating = as.numeric(input$star_seating),
        write_up      = input$review_text
      ))
      
      # Show the spot's reviews page so they can see their new review
      selected_spot(rating_spot())
      rating_spot(0)
      showNotification("Thanks for your review!")
    }
  })
  
  
  # ---- The sidebar: rating form OR reviews OR list ----
  output$sidebar <- renderUI({
    
    if (rating_spot() != 0) {
      
      # ===== FULL-SCREEN RATING FORM =====
      spot <- spots[spots$id == rating_spot(), ]
      
      div(class = "review-fullscreen",
          actionButton("cancel_rating", "\u2190 Back"),
          h2(paste("Review", spot$name)),
          textInput("review_name", "Reviewer name (optional)"),
          star_input("star_wifi",    "Wi-Fi quality"),
          star_input("star_outlet",  "Outlet access"),
          star_input("star_traffic", "Busyness"),
          star_input("star_noise",   "Noise level"),
          star_input("star_seating", "Seating availability"),
          textAreaInput("review_text", "Overall thoughts", width = "100%"),
          actionButton("submit_review", "Submit", class = "solid-btn")
      )
      
    } else if (selected_spot() == 0) {
      
      # ===== LANDING PAGE: list of study spots =====
      tagList(
        h5("Search UCLA study spots"),
        textInput("search", label = NULL, placeholder = "Search by name or area of campus"),
        uiOutput("spot_list")
      )
      
    } else {
      
      # ===== REVIEWS VIEW =====
      data <- all_reviews()
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
    
    data <- all_reviews()
    
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


# -----------------------------------------------------------------------------
# 6. RUN
# -----------------------------------------------------------------------------
shinyApp(ui, server)