library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidytext)
library(lubridate)
library(RColorBrewer)
library(ggmap)

# Function to standardize state names
standardize_state_names <- function(state) {
  state <- tolower(state)
  state <- gsub("\\s*\\-.*", "", state) # Remove anything after a hyphen
  state <- gsub("[^a-z]", "", state)    # Remove non-alphabetic characters
  return(state)
}

# Function to get latitude and longitude
get_lat_lon <- function(location) {
  result <- geocode(location, output = "latlona", source = "google")
  return(result)
}

# UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "NYE Resolutions Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Popular Resolutions", tabName = "popularResolutions", icon = icon("list-alt")),
      menuItem("Retweeted Resolutions", tabName = "retweetedResolutions", icon = icon("retweet")),
      menuItem("Tweet Timing", tabName = "tweetTiming", icon = icon("clock")),
      menuItem("Geographical Analysis", tabName = "geoAnalysis", icon = icon("map-marker-alt")),
      menuItem("User Demographics", tabName = "userDemographics", icon = icon("users")),
      fileInput("file1", "Upload CSV File", accept = c(".csv"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #1f1f1f;
          color: #ffffff;
          font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        }
        .skin-blue .main-header .logo {
          background-color: #1DA1F2;
          color: white;
        }
        .skin-blue .main-header .navbar {
          background-color: #1DA1F2;
        }
        .skin-blue .main-sidebar {
          background-color: #2f2f2f;
          color: #ffffff;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #1DA1F2;
          color: white;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #ffffff;
        }
        .box {
          background: #2f2f2f;
          color: #ffffff;
          border-radius: 10px;
          border: 1px solid #444;
        }
        .box-header {
          background: #1DA1F2;
          color: white;
          border-bottom: 1px solid #444;
        }
        .box-body {
          background: #2f2f2f;
        }
        .plot-container {
          transition: box-shadow 0.3s;
        }
        .plot-container:hover {
          box-shadow: 0 0 10px #1DA1F2;
        }
        .leaflet-container {
          height: 500px;
          width: 100%;
          max-width: 100%;
          max-height: 100%;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "popularResolutions",
              fluidRow(
                box(
                  title = "Most Popular Resolution Categories", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("popularCategoriesPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "retweetedResolutions",
              fluidRow(
                box(
                  title = "Most Retweeted Resolution Categories", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("retweetedCategoriesPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "tweetTiming",
              fluidRow(
                box(
                  title = "Popular Hours for Tweeting Resolutions", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("tweetTimingPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "geoAnalysis",
              fluidRow(
                box(
                  title = "U.S. State with Highest Number of NYE Resolutions", width = 12, status = "primary", solidHeader = TRUE,
                  leafletOutput("stateMap", height = 500)
                )
              )
      ),
      tabItem(tabName = "userDemographics",
              fluidRow(
                box(
                  title = "User Demographics", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("userDemographicsPlot", height = 500)
                )
              )
      )
    )
  )
)

# Server logic for the application
server <- function(input, output, session) {
  
  # Reactive expression to load data and add latitude/longitude
  tweet_data <- reactive({
    req(input$file1)
    data <- read.csv(input$file1$datapath, encoding = "latin1")
    
    # Add latitude and longitude
    data <- data %>%
      mutate(location = paste(tweet_location, tweet_state, sep = ", ")) %>%
      rowwise() %>%
      mutate(geocode = list(get_lat_lon(location))) %>%
      unnest(cols = c(geocode))
    
    return(data)
  })
  
  # Most popular resolution categories
  output$popularCategoriesPlot <- renderPlotly({
    data <- tweet_data()
    popular_categories <- data %>% count(tweet_category, sort = TRUE)
    
    p <- ggplot(popular_categories, aes(x = reorder(tweet_category, n), y = n, fill = tweet_category)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Most Popular Resolution Categories", x = "Category", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Most retweeted resolution categories
  output$retweetedCategoriesPlot <- renderPlotly({
    data <- tweet_data()
    retweeted_categories <- data %>% count(tweet_category, sort = TRUE) %>% arrange(desc(n))
    
    p <- ggplot(retweeted_categories, aes(x = reorder(tweet_category, n), y = n, fill = tweet_category)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Most Retweeted Resolution Categories", x = "Category", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Most popular hours for tweeting resolutions
  output$tweetTimingPlot <- renderPlotly({
    data <- tweet_data()
    data$rounded_hour <- round_date(ymd_hms(data$tweet_created), "hour")
    tweet_hours <- data %>% count(rounded_hour) %>% mutate(hour = hour(rounded_hour))
    
    p <- ggplot(tweet_hours, aes(x = hour, y = n, fill = as.factor(hour))) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Popular Hours for Tweeting Resolutions", x = "Hour of Day", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Map visualization of states with highest number of NYE resolutions
  output$stateMap <- renderLeaflet({
    data <- tweet_data()
    
    # Standardize state names
    data$tweet_state <- standardize_state_names(data$tweet_state)
    
    state_counts <- data %>% count(tweet_state, sort = TRUE)
    
    states <- map_data("state")
    states$region <- standardize_state_names(states$region)
    
    state_counts$state <- standardize_state_names(state_counts$tweet_state)
    
    state_data <- merge(states, state_counts, by.x = "region", by.y = "state", all.x = TRUE)
    state_data$n[is.na(state_data$n)] <- 0
    
    pal <- colorNumeric("Blues", domain = state_data$n)
    
    leaflet(state_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(n), fillOpacity = 0.7, color = "#BDBDC3", weight = 1,
        popup = ~paste("<strong>State:</strong>", region, "<br><strong>Count:</strong>", n)
      ) %>%
      addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Number of Resolutions",
                position = "bottomright")
  })
  
  # User demographics plot
  output$userDemographicsPlot <- renderPlotly({
    data <- tweet_data()
    demographics <- data %>% count(user_gender, sort = TRUE)
    
    p <- ggplot(demographics, aes(x = reorder(user_gender, n), y = n
                                  p <- ggplot(demographics, aes(x = reorder(user_gender, n), y = n, fill = user_gender)) +
                                    geom_bar(stat = "identity") +
                                    coord_flip() +
                                    scale_fill_brewer(palette = "Set2") +
                                    labs(title = "User Gender Distribution", x = "Gender", y = "Count") +
                                    theme_minimal() +
                                    theme(legend.position = "none")
                                  
                                  ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

                                  






    

                                                                 

    
    

