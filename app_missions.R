library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(lubridate)
library(shinyjs)

login_ui <- fluidPage(
  useShinyjs(),
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
        background: rgba(0, 0, 0, 0.8);
        padding: 20px;
        border-radius: 10px;
        display: inline-block;
      }
      .login-box input {
        margin-bottom: 10px;
        border: none;
        padding: 10px;
        border-radius: 5px;
      }
      .login-box button {
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        background-color: #00FFFF;
        color: white;
        font-size: 16px;
        cursor: pointer;
      }
    "))
  ),
  div(class = "login-box",
      h2("Login"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login", "Login")
  )
)

# UI for the dashboard page
dashboard_ui <- dashboardPage(
  dashboardHeader(title = "Space Missions Analysis"),
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
          background: rgba(0, 0, 0, 0.8) !important;
          color: white !important;
        }
      "))
    ),
    fluidRow(
      valueBoxOutput("totalMissionsBox"),
      valueBoxOutput("successfulMissionsBox"),
      valueBoxOutput("failedMissionsBox"),
      valueBoxOutput("uniqueCompaniesBox"),
      valueBoxOutput("avgMissionCostBox"),
      valueBoxOutput("avgMissionsPerYearBox")
    ),
    tabItems(
      tabItem(tabName = "launchTrends",
              fluidRow(
                box(
                  title = "Rocket Launches Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("launchTrendsPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "missionSuccess",
              fluidRow(
                box(
                  title = "Mission Success Rate Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("missionSuccessPlot", height = 500)
                ),
                box(
                  title = "Total Missions vs Successful Missions", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("totalVsSuccessPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "rocketAnalysis",
              fluidRow(
                box(
                  title = "Rocket Usage Over Time", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("rocketUsagePlot", height = 500)
                ),
                box(
                  title = "Top 15 Companies by Missions", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("companyMissionsPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "missionCost",
              fluidRow(
                box(
                  title = "Mission Cost Analysis", width = 12, status = "primary", solidHeader = TRUE,
                  plotlyOutput("missionCostPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "interactiveData",
              fluidRow(
                box(
                  title = "Interactive Data Table", width = 12, status = "primary", solidHeader = TRUE,
                  dataTableOutput("dataTable")
                )
              )
      )
    )
  )
)

# Server logic for the application
server <- function(input, output, session) {
  observeEvent(input$login, {
    if (input$username != "" && input$password != "") {
      shinyjs::hide("login-page")
      shinyjs::show("dashboard-page")
    } else {
      showModal(modalDialog(
        title = "Error",
        "Invalid username or password",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Reactive expression to load data
  space_missions <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, show_col_types = FALSE)
  })
  
  # KPIs
  output$totalMissionsBox <- renderValueBox({
    data <- space_missions()
    total_missions <- nrow(data)
    valueBox(value = total_missions, subtitle = "Total Missions", icon = icon("rocket"), color = "aqua", width = 3)
  })
  
  output$successfulMissionsBox <- renderValueBox({
    data <- space_missions()
    successful_missions <- nrow(data %>% filter(MissionStatus == "Success"))
    valueBox(value = successful_missions, subtitle = "Successful Missions", icon = icon("check"), color = "aqua", width = 3)
  })
  
  output$failedMissionsBox <- renderValueBox({
    data <- space_missions()
    failed_missions <- nrow(data %>% filter(MissionStatus == "Failure"))
    valueBox(value = failed_missions, subtitle = "Failed Missions", icon = icon("times"), color = "aqua", width = 3)
  })
  
  output$uniqueCompaniesBox <- renderValueBox({
    data <- space_missions()
    unique_companies <- n_distinct(data$Company)
    valueBox(value = unique_companies, subtitle = "Unique Companies", icon = icon("building"), color = "aqua", width = 3)
  })
  
  output$avgMissionCostBox <- renderValueBox({
    data <- space_missions()
    avg_cost <- mean(as.numeric(data$Price), na.rm = TRUE)
    valueBox(value = round(avg_cost, 2), subtitle = "Average Mission Cost (USD)", icon = icon("dollar-sign"), color = "aqua", width = 3)
  })
  
  output$avgMissionsPerYearBox <- renderValueBox({
    data <- space_missions()
    avg_missions_per_year <- data %>%
      mutate(year = year(Date)) %>%
      group_by(year) %>%
      summarise(missions = n()) %>%
      summarise(avg_missions = mean(missions))
    valueBox(value = round(avg_missions_per_year$avg_missions), subtitle = "Average Missions per Year", icon = icon("calendar"), color = "aqua", width = 3)
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
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
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
          text = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")
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
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.title = element_blank(),
        legend.position = "top"
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
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
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
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
      )
    
    ggplotly(p)
  })
  
  # Mission Cost Analysis
  output$missionCostPlot <- renderPlotly({
    data <- space_missions() %>%
      filter(!is.na(Price)) %>%
      mutate(Price = as.numeric(Price)) %>%
      group_by(year = year(Date)) %>%
      summarize(total_cost = sum(Price, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = year, y = total_cost)) +
      geom_bar(stat = "identity", fill = "#00FFFF") +
      labs(title = "Total Mission Cost Over Time", x = "Year", y = "Total Cost (in USD)") +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
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

ui_combined <- fluidPage(
  useShinyjs(),
  div(id = "login-page", login_ui),
  shinyjs::hidden(div(id = "dashboard-page", dashboard_ui))
)


shinyApp(ui = ui_combined, server = server)

    

    

    
           
      
      

