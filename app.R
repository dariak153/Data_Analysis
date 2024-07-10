library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Restaurant Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Highest Rated Restaurants", tabName = "highestRated", icon = icon("star")),
      menuItem("Consumer Demographics", tabName = "consumerDemographics", icon = icon("users")),
      menuItem("Demand & Supply Gaps", tabName = "demandSupply", icon = icon("chart-bar")),
      menuItem("Restaurant Database", tabName = "restaurantDB", icon = icon("database")),
      fileInput("consumer_preferences", "Upload consumer_preferences.csv"),
      fileInput("consumers", "Upload consumers.csv"),
      fileInput("ratings", "Upload ratings.csv"),
      fileInput("restaurant_cuisines", "Upload restaurant_cuisines.csv"),
      fileInput("restaurants", "Upload restaurants.csv")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        body {
          background-color: #1F2C34;
          color: #ECF0F1;
        }
        .skin-blue .main-header .logo {
          background-color: #2C3E50;
        }
        .skin-blue .main-header .navbar {
          background-color: #2C3E50;
        }
        .skin-blue .main-sidebar {
          background-color: #2C3E50;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #18BC9C;
          color: #ECF0F1;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #ECF0F1;
        }
        .box {
          background: #34495E;
          color: #ECF0F1;
          border-radius: 10px;
        }
        .box-header {
          background: #2C3E50;
          color: #ECF0F1;
        }
        .box-body {
          background: #34495E;
        }
        .plot-container {
          transition: box-shadow 0.3s;
        }
        .plot-container:hover {
          box-shadow: 0 0 10px #18BC9C;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "highestRated",
              fluidRow(
                box(
                  title = "Top 10 Highest Rated Restaurants", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("highestRatedPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "consumerDemographics",
              fluidRow(
                box(
                  title = "Consumer Age Distribution", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("consumerDemographicsPlot", height = 500)
                )
              ),
              fluidRow(
                box(
                  title = "Cuisine Preferences by City in Mexico", width = 12, status = "primary", solidHeader = TRUE,
                  leafletOutput("cuisineMap", height = 500)
                )
              ),
              fluidRow(
                box(
                  title = "Alcohol Consumption by City", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("alcoholConsumptionPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "demandSupply",
              fluidRow(
                box(
                  title = "Cuisine Popularity (Demand)", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("demandSupplyPlot", height = 500)
                )
              )
      ),
  
      tabItem(tabName = "restaurantDB",
              fluidRow(
                box(
                  title = "Restaurant Database", width = 12, status = "primary", solidHeader = TRUE,
                  DTOutput("restaurantTable")
                )
              ),
              fluidRow(
                box(
                  title = "Restaurant Locations", width = 12, status = "primary", solidHeader = TRUE,
                  leafletOutput("restaurantMap", height = 500)
                )
              )
      )
    )
  )
)
server <- function(input, output) {
  data <- reactive({
    req(input$consumer_preferences)
    req(input$consumers)
    req(input$ratings)
    req(input$restaurant_cuisines)
    req(input$restaurants)
    
    consumer_preferences <- read_csv(input$consumer_preferences$datapath, show_col_types = FALSE)
    consumers <- read_csv(input$consumers$datapath, show_col_types = FALSE)
    ratings <- read_csv(input$ratings$datapath, show_col_types = FALSE)
    restaurant_cuisines <- read_csv(input$restaurant_cuisines$datapath, show_col_types = FALSE)
    restaurants <- read_csv(input$restaurants$datapath, show_col_types = FALSE)
    
    list(
      consumer_preferences = consumer_preferences, 
      consumers = consumers, 
      ratings = ratings, 
      restaurant_cuisines = restaurant_cuisines, 
      restaurants = restaurants
    )
  })
  
  output$highestRatedPlot <- renderPlotly({
    df <- data()
    ratings <- df$ratings
    restaurants <- df$restaurants
    
    highest_rated <- ratings %>%
      group_by(Restaurant_ID) %>%
      summarize(avg_rating = mean(Overall_Rating, na.rm = TRUE)) %>%
      arrange(desc(avg_rating)) %>%
      head(10) %>%
      inner_join(restaurants, by = "Restaurant_ID")
    
    p <- ggplot(highest_rated, aes(x = reorder(Name, avg_rating), y = avg_rating)) +
      geom_bar(stat = "identity", fill = "#18BC9C") +
      coord_flip() +
      labs(title = "Top 10 Highest Rated Restaurants", x = "Restaurant", y = "Average Rating") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2C3E50"),
        panel.background = element_rect(fill = "#34495E"),
        plot.title = element_text(color = "#ECF0F1"),
        axis.title = element_text(color = "#ECF0F1"),
        axis.text = element_text(color = "#ECF0F1")
      )
    
    ggplotly(p)
  })
  
  output$consumerDemographicsPlot <- renderPlotly({
    df <- data()
    consumers <- df$consumers
    
    age_groups <- consumers %>%
      mutate(AgeGroup = case_when(
        Age < 20 ~ "Under 20",
        Age >= 20 & Age < 30 ~ "20-29",
        Age >= 30 & Age < 40 ~ "30-39",
        Age >= 40 & Age < 50 ~ "40-49",
        Age >= 50 & Age < 60 ~ "50-59",
        Age >= 60 & Age < 70 ~ "60-69",
        Age >= 70 ~ "70+"
      )) %>%
      count(AgeGroup)
    
    p <- plot_ly(age_groups, labels = ~AgeGroup, values = ~n, type = 'pie', 
                 textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                 marker = list(colors = c("#32CD32", brewer.pal(6, "Greens")),
                               line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Consumer Age Distribution', 
             paper_bgcolor = '#2C3E50', plot_bgcolor = '#34495E',
             titlefont = list(color = '#ECF0F1'), 
             font = list(color = '#ECF0F1'))
    
    p
  })
  
  output$cuisineMap <- renderLeaflet({
    df <- data()
    consumers <- df$consumers %>%
      filter(Country == "Mexico")
    
    consumer_preferences <- df$consumer_preferences %>%
      inner_join(consumers, by = "Consumer_ID")
    
    cuisine_city_counts <- consumer_preferences %>%
      group_by(City, Preferred_Cuisine) %>%
      summarise(count = n(), Latitude = first(Latitude), Longitude = first(Longitude), .groups = 'drop')
    
    palette_colors <- colorRampPalette(brewer.pal(11, "Spectral"))(length(unique(cuisine_city_counts$Preferred_Cuisine)))
    pal <- colorFactor(palette = palette_colors, domain = cuisine_city_counts$Preferred_Cuisine)
    
    leaflet(cuisine_city_counts) %>%
      addTiles() %>%
      setView(lng = -102.5528, lat = 23.6345, zoom = 5) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, color = ~pal(Preferred_Cuisine),
        radius = ~sqrt(count) * 2, stroke = FALSE, fillOpacity = 0.7,
        popup = ~paste0("<strong>City: </strong>", City, "<br><strong>Cuisine: </strong>", Preferred_Cuisine, "<br><strong>Count: </strong>", count)
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, values = ~Preferred_Cuisine,
        title = "Cuisine Type", opacity = 1,
        labFormat = labelFormat(
          transform = function(x) {sort(x, decreasing = TRUE)}
        ),
        bins = length(unique(cuisine_city_counts$Preferred_Cuisine))
      )
  })
  
  output$demandSupplyPlot <- renderPlotly({
    df <- data()
    ratings <- df$ratings
    restaurant_cuisines <- df$restaurant_cuisines
    
    cuisine_ratings <- ratings %>%
      inner_join(restaurant_cuisines, by = "Restaurant_ID") %>%
      group_by(Cuisine) %>%
      summarize(avg_rating = mean(Overall_Rating, na.rm = TRUE), count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    
    p <- ggplot(cuisine_ratings, aes(x = reorder(Cuisine, count), y = count)) +
      geom_bar(stat = "identity", fill = "#18BC9C") +
      coord_flip() +
      labs(title = "Cuisine Popularity (Demand)", x = "Cuisine", y = "Number of Ratings") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2C3E50"),
        panel.background = element_rect(fill = "#34495E"),
        plot.title = element_text(color = "#ECF0F1"),
        axis.title = element_text(color = "#ECF0F1"),
        axis.text = element_text(color = "#ECF0F1")
      )
    
    ggplotly(p)
  })
  
  output$alcoholConsumptionPlot <- renderPlotly({
    df <- data()
    consumers <- df$consumers %>%
      filter(Country == "Mexico")
    
    alcohol_consumption <- consumers %>%
      group_by(City, Drink_Level) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(alcohol_consumption, aes(x = reorder(City, count), y = count, fill = Drink_Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Alcohol Consumption by City", x = "City", y = "Number of Consumers", fill = "Drink Level") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2C3E50"),
        panel.background = element_rect(fill = "#34495E"),
        plot.title = element_text(color = "#ECF0F1"),
        axis.title = element_text(color = "#ECF0F1"),
        axis.text = element_text(color = "#ECF0F1"),
        legend.background = element_rect(fill = "#2C3E50"),
        legend.title = element_text(color = "#ECF0F1"),
        legend.text = element_text(color = "#ECF0F1")
      ) +
      scale_fill_brewer(palette = "Paired")
    
    ggplotly(p)
  })
  
  output$investmentTable <- renderTable({
    df <- data()
    ratings <- df$ratings
    restaurants <- df$restaurants
    
    investment_char <- ratings %>%
      group_by(Restaurant_ID) %>%
      summarize(avg_rating = mean(Overall_Rating, na.rm = TRUE), count = n(), .groups = 'drop') %>%
      inner_join(restaurants, by = "Restaurant_ID") %>%
      arrange(desc(avg_rating)) %>%
      head(10) %>%
      select(Name, avg_rating, count, City)
    
    investment_char
  })
  
  output$restaurantTable <- renderDT({
    df <- data()
    restaurants <- df$restaurants
    
    datatable(restaurants, options = list(
      pageLength = 10,
      autoWidth = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), extensions = 'Buttons', rownames = FALSE)
  })
  
  output$restaurantMap <- renderLeaflet({
    df <- data()
    restaurants <- df$restaurants
    
    leaflet(restaurants) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude,  
        popup = ~paste0("<strong>Name: </strong>", Name, "<br><strong>City: </strong>", City)
      ) %>%
      setView(lng = -102.5528, lat = 23.6345, zoom = 5)
  })
}

shinyApp(ui, server)







    

                                                                 

    
    

