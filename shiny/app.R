library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(tm)
library(wordcloud)
library(SnowballC)
library(stopwords)
generate_insights <- function(selected_business_id, bars_day_op_data, bestdays_data, topbar_attrs_data) {
  # Find the state for the selected business
  business_state <- subset(bars_day_op_data, business_id == selected_business_id)$state
  # Ensure the business exists in the data
  if (length(business_state) == 0) {
    return("Selected business not found in the data.")
  }
  
  # Get the top traffic days for that state
  top_traffic_days <- bestdays_data[bestdays_data$state == business_state, 1:3]
  # Check if the business is open on these days
  business_days_open <- bars_day_op_data[bars_day_op_data$business_id == selected_business_id, 3:9]
  # Determine if the business is open on all top traffic days
  is_open_on_top_days <- all(top_traffic_days %in% names(business_days_open))
  # Generate insights
  if (is_open_on_top_days) {
    insights <- "You're open on all high traffic days."
  } else {
    missing_days <- top_traffic_days[!top_traffic_days %in% names(business_days_open)[business_days_open]]
    insights <- paste("You're not open on", paste(missing_days, collapse=", "), "which has about 9-16% higher traffic.")
  }
  business_attrs <- topbar_attrs_data[topbar_attrs_data$business_id == selected_business_id,]
  ambience_insights <- ""
  if (business_attrs$hipster == 0 && business_attrs$intimate == 0 && business_attrs$upscale == 0) {
    ambience_insights <- "Consider adopting Ambience like intimate, upscale, and hipster which could potentially lead to an increase of 0.5 stars on average."
  } else {
    ambience_insights <- "Your current Ambience are aligned with higher mean ratings."
  }
  noise_insights = ""
  if (business_attrs$NoiseLevel == 1 || business_attrs$NoiseLevel == 2) {
    noise_insights <- "Consider reducing Noise Level inside the bar."
  } else {
    noise_insights <- "Your current Noise Levels are aligned with higher mean ratings."
  }
  tv_insights = ""
  if (business_attrs$HasTV == 1) {
    tv_insights <- "Consider removing TVs for more sociable environment."
  } else {
    tv_insights <- "Not having TVs is compatible with higher ratings in bars"
  }
  group_insights = ""
  if (business_attrs$RestaurantsGoodForGroups == 0) {
    tv_insights <- "Adding tables or booths for Larger Groups might increase higher ratings."
  } else {
    tv_insights <- "Your bar is good for groups.\n"
  }
  combined_insights <- paste(insights, ambience_insights, noise_insights, tv_insights, group_insights, sep = "\n")
  return(combined_insights)
}
ui <- fluidPage(
  titlePanel("Yelp Business Analysis"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("locationType", "Choose Location Type:",
                   choices = c("Zip Code" = "zip")),
      uiOutput("locationInput"),
      uiOutput("businessInput"), # UI for business selection
      selectInput("dayOfWeek", "Select Day of the Week:",
                  choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
      tags$hr()
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trends",
                 plotOutput("trendPlot", brush = "plot_brush"),
                 verbatimTextOutput("statsOutput"),
                 dataTableOutput("reviewText")
                 # Output for descriptive statistics
        ),
        tabPanel("Insights",
                 plotOutput("wordCloudPlot"),        # Placeholder for word cloud
                 verbatimTextOutput("businessInsights") # Placeholder for business insights
        ),
        tabPanel("Contact Us",
          h4("Application Developed and Maintained by:"),
          tags$p("Name: Kai Shukla, Feiyun Yan & Xingyu Tang"),
          tags$p("Email: kshukla2@wisc.edu, fyan27@wisc.edu, xtang244@wisc.edu")
        )
      ) # Output for descriptive statistics
    )
  )
)
server <- function(input, output, session) {
  # Read merged data
  merged_df <- read.csv("https://raw.githubusercontent.com/Kaiwalya2502/yelp_data_analysis/main/camerged_data.csv") # Replace with the actual path to your file
  # Dynamic UI for location input
  output$locationInput <- renderUI({
    if(input$locationType == "zip") {
      selectInput("location", "Select Zip Code:", choices = unique(merged_df$postal_code))
    } else {
      selectInput("location", "Select County:", choices = unique(merged_df$county))
    }
  })
  # Dynamic UI for business input
  output$businessInput <- renderUI({
    req(input$location) # Ensure a location is selected
    location_col <- if(input$locationType == "zip") "postal_code" else "county"
    selectInput("business", "Select Business:",
                choices = unique(merged_df$name[merged_df[[location_col]] == input$location]))
  })
  # Reactive data for trend plot and individual reviews
  trendData <- reactive({
    req(input$location, input$business) # Ensure location and business are selected
    location_col <- if(input$locationType == "zip") "postal_code" else "county"
    df <- subset(merged_df, merged_df[[location_col]] == input$location & day_of_week == input$dayOfWeek)
    business_reviews <- subset(merged_df, merged_df$name == input$business & merged_df[[location_col]] == input$location & day_of_week == input$dayOfWeek)
    list(aggregate = aggregate(stars_y ~ hour, data = df, FUN = mean),
         individual = business_reviews)
  })
  # Trend plot with individual business reviews
  output$trendPlot <- renderPlot({
    data <- trendData()
    # Check if the data is valid and not empty
    if(!is.null(data) && !is.null(data$aggregate) && nrow(data$aggregate) > 0) {
      p <- ggplot(data$aggregate, aes(x = hour, y = stars_y)) +
        geom_line(color = "blue") # Average trend line
      # Add individual reviews if available
      if(!is.null(data$individual) && nrow(data$individual) > 0) {
        p <- p + geom_point(data = data$individual, aes(x = hour, y = stars_y, color = as.factor(business_id)), size = 3)
      }
      p + labs(title = "Average Stars Trend with Individual Business Reviews", x = "Hour of Review", y = "Average Stars") +
        theme_classic()
    } else {
      plot.new()
      title(main = "No data to display")
    }
  })
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      # Access the correct data from the reactive trendData
      brushed_data <- brushedPoints(trendData()$individual, brush)
      if (!is.null(brushed_data) && nrow(brushed_data) > 0) {
        # Update the review text based on brushed points
        output$reviewText <- renderDataTable({
          brushed_data[, c("text"), drop = FALSE]
        })
        reviews_text <- paste(brushed_data$text, collapse = " ")
        business_name_words <- unlist(strsplit(input$business, " ")) 
        custom_stop_words = c(stopwords("en"), business_name_words, stopwords("en", source = "snowball"))
        # Process the text for word cloud
        docs <- Corpus(VectorSource(reviews_text))
        docs <- tm_map(docs, content_transformer(tolower))
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, removeWords, custom_stop_words)
        docs <- tm_map(docs, stripWhitespace)
        docs <- docs[sapply(docs, length) > 0]  # Remove empty documents
        
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        word_freqs <- sort(rowSums(m), decreasing = TRUE)
        df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)
        
        # Generate word cloud
        output$wordCloudPlot <- renderPlot({
          wordcloud(words = df_word_freqs$word, freq = df_word_freqs$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"), scale = c(4, 0.5))  # Adjusted scale
        })
        
        
      }
    }
  })
  # Descriptive statistics output
  output$statsOutput <- renderText({
    req(input$location, input$business) # Ensure location and business are selected
    location_col <- if(input$locationType == "zip") "postal_code" else "county"
    # Business-specific statistics
    business_data <- subset(merged_df, merged_df$name == input$business & merged_df[[location_col]] == input$location)
    business_avg_stars <- mean(business_data$stars_y, na.rm = TRUE)
    total_reviews_business <- nrow(business_data)
    # Location-specific statistics
    location_data <- subset(merged_df, merged_df[[location_col]] == input$location)
    location_avg_stars <- mean(location_data$stars_y, na.rm = TRUE)
    # Day of week statistics
    day_data <- subset(business_data, day_of_week == input$dayOfWeek)
    total_reviews_day <- nrow(day_data)
    paste("Business Average Stars: ", round(business_avg_stars, 2),
          "\nAverage Stars in Selected Location: ", round(location_avg_stars, 2),
          "\nTotal Reviews for Selected Business: ", total_reviews_business,
          "\nTotal Reviews for Day of the Week for Selected Business: ", total_reviews_day)
  })
  output$businessInsights <- renderText({
    req(input$business)
    business_id = subset(merged_df, name == input$business)$business_id[1]
    paste(generate_insights(business_id, business_op_data, bestdays_data, topattrs_data))
  })
}
business_op_data = read.csv("https://raw.githubusercontent.com/Kaiwalya2502/yelp_data_analysis/main/bars_day_op_data.csv")
bestdays_data = read.csv("https://raw.githubusercontent.com/Kaiwalya2502/yelp_data_analysis/main/bestdays_data.csv")
topattrs_data = read.csv("https://raw.githubusercontent.com/Kaiwalya2502/yelp_data_analysis/main/topbar_attrs_data.csv")
# Run the app
shinyApp(ui = ui, server = server)
