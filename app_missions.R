library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(shinyjs)

# UI for the application
dashboard_ui <- dashboardPage(
  dashboardHeader(
    title = "Space Missions Analysis",
    tags$li(class = "dropdown",
            tags$a(href = "https://www.linkedin.com/shareArticle?mini=true&url=http://yourappurl.com", 
                   target = "_blank", class = "btn btn-social-icon btn-linkedin", icon("linkedin"))),
    tags$li(class = "dropdown",
            tags$a(href = "https://www.facebook.com/sharer/sharer.php?u=http://yourappurl.com", 
                   target = "_blank", class = "btn btn-social-icon btn-facebook", icon("facebook"))),
    tags$li(class = "dropdown",
            tags$a(href = "https://twitter.com/intent/tweet?url=http://yourappurl.com&text=Check%20out%20this%20cool%20app!", 
                   target = "_blank", class = "btn btn-social-icon btn-twitter", icon("twitter")))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Launch Trends", tabName = "launchTrends", icon = icon("rocket")),
      menuItem("Mission Success", tabName = "missionSuccess", icon = icon("check-circle")),
      menuItem("Rocket Analysis", tabName = "rocketAnalysis", icon = icon("space-shuttle")),
      menuItem("Mission Cost", tabName = "missionCost", icon = icon("dollar-sign")),
      menuItem("Interactive Data", tabName = "interactiveData", icon = icon("table")),
      fileInput("file1", "Upload CSV File", accept = c(".csv"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
          background-image: url('https://www.nasa.gov/wp-content/uploads/2022/07/web_first_images_release.png');
          background-size: cover;
          background-attachment: fixed;
          color: white;
        }
        .main-header .logo, .main-header .navbar, .main-sidebar, .sidebar-menu .active a, .sidebar-menu a {
          background-color: transparent !important;
          color: white !important;
        }
        .box {
          background: rgba(0, 0, 0, 0.8) !important;
          color: #ffffff;
          border-radius: 10px;
          border: 1px solid #444;
        }
        .box-header {
          background: rgba(0, 0, 0, 0.8) !important;
          color: white !important;
          border-bottom: 1px solid #444;
        }
        .box-body {
          background: rgba(0, 0, 0, 0.8) !important;
        }
        .value-box {
          background: rgba(0, 0, 51, 0.8) !important;
          color: white !important;
          border-radius: 10px;
          border: 1px solid #00FFFF;
          box-shadow: 0 0 10px rgba(0, 255, 255, 0.5);
          transition: transform 0.3s, box-shadow 0.3s;
          cursor: pointer;
          padding: 20px;
          text-align: center;
          margin-bottom: 20px;
        }
        .value-box:hover {
          transform: scale(1.05);
          box-shadow: 0 0 15px rgba(0, 255, 255, 0.8);
          background: rgba(0, 0, 102, 0.8) !important;
        }
        .value-box h5, .value-box p {
          color: white !important;
          margin: 0;
        }
        .fa-icon {
          font-size: 1.5em;
          margin-bottom: 10px;
        }
      "))
    ),
    fluidRow(
      column(width = 4,
             div(class = "value-box",
                 icon("rocket", class = "fa-icon"),
                 h5(textOutput("totalMissionsBox")),
                 p("Total Missions"))),
      column(width = 4,
             div(class = "value-box",
                 icon("check-circle", class = "fa-icon"),
                 h5(textOutput("successfulMissionsBox")),
                 p("Successful"))),
      column(width = 4,
             div(class = "value-box",
                 icon("times-circle", class = "fa-icon"),
                 h5(textOutput("failedMissionsBox")),
                 p("Failed"))),
      column(width = 4,
             div(class = "value-box",
                 icon("building", class = "fa-icon"),
                 h5(textOutput("uniqueCompaniesBox")),
                 p("Companies"))),
      column(width = 4,
             div(class = "value-box",
                 icon("space-shuttle", class = "fa-icon"),
                 h5(textOutput("uniqueRocketsBox")),
                 p("Rockets"))),
      column(width = 4,
             div(class = "value-box",
                 icon("calendar-alt", class = "fa-icon"),
                 h5(textOutput("avgMissionsPerYearBox")),
                 p("Avg/Year")))
    ),
    tabItems(
      tabItem(tabName = "launchTrends",
              fluidRow(
                box(
                  title = "Rocket Launches Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("chart-line"),
                  plotlyOutput("launchTrendsPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "missionSuccess",
              fluidRow(
                box(
                  title = "Mission Success Rate Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("chart-bar"),
                  plotlyOutput("missionSuccessPlot", height = 500)
                ),
                box(
                  title = "Total Missions vs Successful Missions", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("chart-area"),
                  plotlyOutput("totalVsSuccessPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "rocketAnalysis",
              fluidRow(
                box(
                  title = "Rocket Usage Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("rocket"),
                  plotlyOutput("rocketUsagePlot", height = 500)
                ),
                box(
                  title = "Top 15 Companies by Missions", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("building"),
                  plotlyOutput("companyMissionsPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "missionCost",
              fluidRow(
                box(
                  title = "Mission Cost Analysis", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("dollar-sign"),
                  plotlyOutput("missionCostPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "interactiveData",
              fluidRow(
                box(
                  title = "Interactive Data Table", width = 12, status = "primary", solidHeader = TRUE,
                  icon = icon("table"),
                  dataTableOutput("dataTable")
                )
              )
      )
    )
  )
)

# Server logic for the application
dashboard_server <- function(input, output, session) {
  # Reactive expression to load data
  space_missions <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  
  # KPIs
  output$totalMissionsBox <- renderText({
    data <- space_missions()
    total_missions <- nrow(data)
    total_missions
  })
  
  output$successfulMissionsBox <- renderText({
    data <- space_missions()
    successful_missions <- nrow(data %>% filter(MissionStatus == "Success"))
    successful_missions
  })
  
  output$failedMissionsBox <- renderText({
    data <- space_missions()
    failed_missions <- nrow(data %>% filter(MissionStatus == "Failure"))
    failed_missions
  })
  
  output$uniqueCompaniesBox <- renderText({
    data <- space_missions()
    unique_companies <- n_distinct(data$Company)
    unique_companies
  })
  
  output$uniqueRocketsBox <- renderText({
    data <- space_missions()
    unique_rockets <- n_distinct(data$Rocket)
    unique_rockets
  })
  
  output$avgMissionsPerYearBox <- renderText({
    data <- space_missions() %>% mutate(year = year(Date))
    avg_missions_per_year <- data %>% group_by(year) %>% summarise(missions = n()) %>% summarise(avg = mean(missions)) %>% pull(avg)
    round(avg_missions_per_year)
  })
  
  # Rocket Launches Over Time
  output$launchTrendsPlot <- renderPlotly({
    data <- space_missions() %>%
      mutate(year = year(Date)) %>%
      group_by(year) %>%
      summarize(launches = n())
    
    p <- ggplot(data, aes(x = year, y = launches)) +
      geom_bar(stat = "identity", fill = "#00FFFF") +
      labs(title = "Rocket Launches Over Time", x = "Year", y = "Number of Launches") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Mission Success Rate Over Time
  output$missionSuccessPlot <- renderPlotly({
    data <- space_missions() %>%
      mutate(year = year(Date)) %>%
      group_by(year) %>%
      summarize(success_rate = mean(MissionStatus == "Success"))
    
    p <- ggplot(data, aes(x = year, y = success_rate)) +
      geom_line(color = "#00FFFF", size = 1) +
      geom_point(color = "#00FFFF", size = 2) +
      labs(title = "Mission Success Rate Over Time", x = "Year", y = "Success Rate") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Total Missions vs Successful Missions Over Time
  output$totalVsSuccessPlot <- renderPlotly({
    data <- space_missions() %>%
      mutate(year = year(Date)) %>%
      group_by(year) %>%
      summarize(
        total_missions = n(),
        successful_missions = sum(MissionStatus == "Success")
      )
    
    p <- ggplot(data) +
      geom_line(aes(x = year, y = total_missions, color = "Total Missions"), size = 1) +
      geom_line(aes(x = year, y = successful_missions, color = "Successful Missions"), size = 1) +
      labs(title = "Total Missions vs Successful Missions Over Time", x = "Year", y = "Number of Missions") +
      scale_color_manual(values = c("Total Missions" = "#FFBB00", "Successful Missions" = "#00FFFF")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Rocket Usage Over Time
  output$rocketUsagePlot <- renderPlotly({
    data <- space_missions() %>%
      count(Rocket, sort = TRUE) %>%
      top_n(15, n)
    
    p <- ggplot(data, aes(x = reorder(Rocket, n), y = n)) +
      geom_bar(stat = "identity", fill = "#00FFFF") +
      coord_flip() +
      labs(title = "Top 15 Rockets by Usage", x = "Rocket", y = "Number of Missions") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Top 15 Companies by Missions
  output$companyMissionsPlot <- renderPlotly({
    data <- space_missions() %>%
      count(Company, sort = TRUE) %>%
      top_n(15, n)
    
    p <- ggplot(data, aes(x = reorder(Company, n), y = n)) +
      geom_bar(stat = "identity", fill = "#00FFFF") +
      coord_flip() +
      labs(title = "Top 15 Companies by Missions", x = "Company", y = "Number of Missions") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Mission Cost Analysis
  output$missionCostPlot <- renderPlotly({
    data <- space_missions() %>%
      filter(!is.na(Price)) %>%
      group_by(year = year(Date)) %>%
      summarize(total_cost = sum(Price, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = year, y = total_cost)) +
      geom_bar(stat = "identity", fill = "#00FFFF") +
      labs(title = "Total Mission Cost Over Time", x = "Year", y = "Total Cost (in USD)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Interactive Data Table
  output$dataTable <- renderDataTable({
    space_missions()
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ), extensions = 'Buttons')
}

# UI for the login page
login_ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://www.nasa.gov/wp-content/uploads/2022/07/web_first_images_release.png');
        background-size: cover;
        background-attachment: fixed;
        color: white;
        text-align: center;
        padding-top: 200px;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .login-box {
        background: rgba(0, 0, 51, 0.8);
        padding: 20px;
        border-radius: 10px;
        display: inline-block;
        box-shadow: 0 0 15px rgba(0, 255, 255, 0.8);
      }
      .login-box input {
        margin-bottom: 10px;
        border: none;
        padding: 10px;
        border-radius: 5px;
        width: 100%;
      }
      .login-box button {
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        background-color: #00FFFF;
        color: white;
        font-size: 16px;
        cursor: pointer;
        width: 100%;
      }
      .login-box button:hover {
        background-color: #00BFFF;
      }
      .login-title {
        font-size: 2em;
        margin-bottom: 20px;
      }
    "))
  ),
  div(class = "login-box",
      div(class = "login-title", "Login"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login", "Login")
  )
)

# Server logic for the login page
login_server <- function(input, output, session) {
  observeEvent(input$login, {
    shiny::removeModal()
    shiny::showModal(modalDialog(
      title = "Login Successful",
      "Welcome to the Space Missions Analysis Dashboard!",
      easyClose = TRUE,
      footer = NULL
    ))
    
    updateTabItems(session, "tabs", "dashboard")
  })
}

# Combine the UI and server logic for login and main application
ui_combined <- fluidPage(
  shinyjs::useShinyjs(),
  div(id = "login-page", login_ui),
  shinyjs::hidden(div(id = "dashboard-page", dashboard_ui))
)

server_combined <- function(input, output, session) {
  login_server(input, output, session)
  dashboard_server(input, output, session)
  
  observeEvent(input$login, {
    shinyjs::hide(id = "login-page")
    shinyjs::show(id = "dashboard-page")
  })
}

# Run the application 
shinyApp(ui = ui_combined, server = server_combined)

    

    

    
           
      
      

