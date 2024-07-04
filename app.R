
install.packages("shiny")
library(shiny)
install.packages(c("ggplot2", "plotly", "dplyr", "readr", "leaflet", "sf", "rnaturalearth", "rnaturalearthdata", "lubridate", "tidyr", "shiny.fluent", "imola", "DT", "fontawesome"))
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(tidyr)
library(shiny.fluent)
library(imola)
library(DT)
library(fontawesome)
install.packages("blastula")
library(shiny)
library(blastula)


color_palette <- c("#1E90FF", "#00008B", "#00FFFF", "#8A2BE2", "#7B68EE", "#00FF7F", "#2E8B57")
kpi_color <- "#1E90FF" 

# Header
header_commandbar_list <- list(
  list(
    key = 'refresh', 
    text = "Refresh Data", 
    iconProps = list(iconName = "Refresh"),
    style = "margin-left: auto;"
  )
)

app_header <- flexPanel(
  id = "header",
  align_items = "center",
  flex = c(0, 1, 0),
  div(
    Text(variant = "xLarge", "| Marketing Data Visualization", style="color: white; display: inline-block; white-space: nowrap;"), 
    style = "margin-bottom: 0; display: flex; align-items: center; width: 100%; justify-content: center;"
  ),
  CommandBar(items = header_commandbar_list),
  style = paste("box-shadow: 0 0 10px #000; background: linear-gradient(90deg, ", kpi_color, " 0%, #00008B 100%); padding: 20px; color: white; display: flex; justify-content: space-between; align-items: center;", sep="")
)

# Sidebar
app_sidebar <- div(
  id = "sidebar",
  style = "background-color: #ECF0F1; padding: 20px; border-radius: 10px;",
  fileInput("file1", "Choose CSV File", accept = c(".csv")),
  selectInput("sector", "Choose Sector:", choices = c("All")),
  selectInput("event_type", "Choose Event Type:", choices = c("All")),
  selectInput("age_group", "Choose Age Group:", choices = c("All", "Under 30", "30-40", "40-50", "50+")),
  dateRangeInput("date_range", "Select Date Range:", start = Sys.Date() - 30, end = Sys.Date()),
  actionButton("update", "Update", style = paste("background-color:", kpi_color, "; color: white; border: none; border-radius: 5px;"))
)

# Footer
app_footer <- flexPanel(
  id = "footer",
  justify_content = 'center',
  gap = "20px",
  style = "background-color: #ECF0F1; padding: 10px; border-top: 1px solid #ddd;"
)

# Summary Cards
summary_card <- function(title, value) {
  div(
    class = "summary-card",
    div(
      class = "summary-card-content",
      div(
        class = "summary-card-value",
        Text(variant = "xxLarge", value, style = paste("color: white; font-weight: bold; background-color:", kpi_color, "; padding: 10px; border-radius: 5px;"))
      ),
      div(
        class = "summary-card-title",
        Text(variant = "medium", title, style = "color: #2C3E50;")
      )
    ),
    style = paste("border: 2px solid", kpi_color, "; border-radius: 5px; padding: 10px;")
  )
}

# Define UI
ui <- gridPage(
  tags$head(
    tags$link(rel="stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
      }
      #header {
        color: white;
        padding: 20px;
        border-radius: 10px;
      }
      #header img {
        border-radius: 50%;
        box-shadow: 0 0 10px rgba(0,0,0,0.5);
      }
      #sidebar {
        background-color: #ECF0F1;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
      #content {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .command-bar {
        float: right;
      }
      .action-button {
        background-color: #1E90FF;
        color: white;
        border-radius: 5px;
        padding: 10px 20px;
        font-size: 16px;
        transition: background-color 0.3s;
      }
      .action-button:hover {
        background-color: #00008B;
      }
      .btn-primary {
        background-color: #1E90FF;
        border-color: #1E90FF;
      }
      .btn-primary:hover {
        background-color: #00008B;
        border-color: #00008B;
      }
      .summary-card {
        display: flex;
        align-items: center;
        background-color: #ffffff;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        margin: 10px;
        text-align: center;
        border: 2px solid #1E90FF;
        border-radius: 5px;
      }
      .summary-card-content {
        flex: 1;
      }
      .summary-card-value {
        font-size: 32px;
        font-weight: bold;
        color: #ffffff;
        background-color: #1E90FF;
        padding: 10px;
        border-radius: 5px;
      }
      .summary-card-title {
        font-size: 18px;
        color: #2C3E50;
      }
      .data-table-container {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        margin-top: 10px;
      }
      .sharing-buttons {
        display: flex;
        justify-content: center;
        padding-top: 10px;
      }
      .sharing-buttons a {
        margin-right: 10px;
      }
    "))
  ),
  template = "grail-left-sidebar",
  gap = "10px",
  
  header = app_header,
  sidebar = app_sidebar,
  content = div(
    id = "content",
    fluidRow(
      column(3, uiOutput("total_customers_card")),
      column(3, uiOutput("average_age_card")),
      column(3, uiOutput("total_sales_card")),
      column(3, uiOutput("total_countries_card"))
    ),
    tabsetPanel(
      id = "tabs",
      tabPanel("Pie Charts",
               fluidRow(
                 column(6, plotlyOutput("education_distribution")),
                 column(6, plotlyOutput("average_age_plot"))
               ),
               div(class = "sharing-buttons",
                   a(href = "https://www.linkedin.com/sharing/share-offsite/?url=http://127.0.0.1:5889/pie_charts", target = "_blank", 
                     icon("linkedin", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.facebook.com/sharer/sharer.php?u=http://127.0.0.1:5889/pie_charts", target = "_blank", 
                     icon("facebook", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.instagram.com/?url=http://127.0.0.1:5889/pie_charts", target = "_blank", 
                     icon("instagram", class = "fa-2x", lib = "font-awesome")),
                   a(href = "mailto:?subject=Marketing%20Data%20Visualization&body=Check%20out%20this%20dashboard:%20http://127.0.0.1:5889/pie_charts", 
                     icon("envelope", class = "fa-2x", lib = "font-awesome"))
               )),
      tabPanel("Bar Charts",
               fluidRow(
                 column(12, plotlyOutput("marital_kidhome_bar_chart")),
                 column(12, plotlyOutput("spending_analysis_plot")),
                 column(12, plotlyOutput("purchase_methods_plot"))
               ),
               div(class = "sharing-buttons",
                   a(href = "https://www.linkedin.com/sharing/share-offsite/?url=http://127.0.0.1:5889/bar_charts", target = "_blank", 
                     icon("linkedin", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.facebook.com/sharer/sharer.php?u=http://127.0.0.1:5889/bar_charts", target = "_blank", 
                     icon("facebook", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.instagram.com/?url=http://127.0.0.1:5889/bar_charts", target = "_blank", 
                     icon("instagram", class = "fa-2x", lib = "font-awesome")),
                   a(href = "mailto:?subject=Marketing%20Data%20Visualization&body=Check%20out%20this%20dashboard:%20http://127.0.0.1:5889/bar_charts", 
                     icon("envelope", class = "fa-2x", lib = "font-awesome"))
               )),
      tabPanel("Line Charts",
               fluidRow(
                 column(12, plotlyOutput("sales_trend_plot")),
                 column(12, plotlyOutput("customer_growth_plot"))
               ),
               div(class = "sharing-buttons",
                   a(href = "https://www.linkedin.com/sharing/share-offsite/?url=http://127.0.0.1:5889/line_charts", target = "_blank", 
                     icon("linkedin", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.facebook.com/sharer/sharer.php?u=http://127.0.0.1:5889/line_charts", target = "_blank", 
                     icon("facebook", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.instagram.com/?url=http://127.0.0.1:5889/line_charts", target = "_blank", 
                     icon("instagram", class = "fa-2x", lib = "font-awesome")),
                   a(href = "mailto:?subject=Marketing%20Data%20Visualization&body=Check%20out%20this%20dashboard:%20http://127.0.0.1:5889/line_charts", 
                     icon("envelope", class = "fa-2x", lib = "font-awesome"))
               )),
      tabPanel("Country Distribution", 
               leafletOutput("country_map"),
               div(class = "sharing-buttons",
                   a(href = "https://www.linkedin.com/sharing/share-offsite/?url=http://127.0.0.1:5889/country_distribution", target = "_blank", 
                     icon("linkedin", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.facebook.com/sharer/sharer.php?u=http://127.0.0.1:5889/country_distribution", target = "_blank", 
                     icon("facebook", class = "fa-2x", lib = "font-awesome")),
                   a(href = "https://www.instagram.com/?url=http://127.0.0.1:5889/country_distribution", target = "_blank", 
                     icon("instagram", class = "fa-2x", lib = "font-awesome")),
                   a(href = "mailto:?subject=Marketing%20Data%20Visualization&body=Check%20out%20this%20dashboard:%20http://127.0.0.1:5889/country_distribution", 
                     icon("envelope", class = "fa-2x", lib = "font-awesome"))
               )),
      tabPanel("Data Table", div(
        class = "data-table-container",
        DTOutput("data_table"),
        div(class = "sharing-buttons",
            a(href = "https://www.linkedin.com/sharing/share-offsite/?url=http://127.0.0.1:5889/data_table", target = "_blank", 
              icon("linkedin", class = "fa-2x", lib = "font-awesome")),
            a(href = "https://www.facebook.com/sharer/sharer.php?u=http://127.0.0.1:5889/data_table", target = "_blank", 
              icon("facebook", class = "fa-2x", lib = "font-awesome")),
            a(href = "https://www.instagram.com/?url=http://127.0.0.1:5889/data_table", target = "_blank", 
              icon("instagram", class = "fa-2x", lib = "font-awesome")),
            a(href = "mailto:?subject=Marketing%20Data%20Visualization&body=Check%20out%20this%20dashboard:%20http://127.0.0.1:5889/data_table", 
              icon("envelope", class = "fa-2x", lib = "font-awesome"))
        )
      ))
    ),
    actionButton("send_email", "Send Email Notification", style = paste("background-color:", kpi_color, "; color: white; border: none; border-radius: 5px;"))
  ),
  footer = app_footer
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    df <- read_csv(input$file1$datapath, col_types = cols(Dt_Customer = col_date(format = "%Y-%m-%d")))
    df <- df %>% mutate(Age = 2024 - Year_Birth)
    
    
    df <- df %>% mutate(Age_Group = case_when(
      Age < 30 ~ "Under 30",
      Age >= 30 & Age <= 40 ~ "30-40",
      Age > 40 & Age <= 50 ~ "40-50",
      Age > 50 ~ "50+"
    ))
    df
  })
  
  observe({
    df <- data()
    updateSelectInput(session, "sector", choices = c("All", unique(df$Marital_Status)))
    updateSelectInput(session, "event_type", choices = c("All", unique(df$Education)))
  })
  
  filtered_data <- reactive({
    df <- data()
    
    if (input$sector != "All") {
      df <- df %>% filter(Marital_Status == input$sector)
    }
    
    if (input$event_type != "All") {
      df <- df %>% filter(Education == input$event_type)
    }
    
    if (input$age_group != "All") {
      df <- df %>% filter(Age_Group == input$age_group)
    }
    
    df <- df %>% filter(Dt_Customer >= input$date_range[1] & Dt_Customer <= input$date_range[2])
    
    df
  })
  
  output$total_customers_card <- renderUI({
    df <- filtered_data()
    total_customers <- n_distinct(df$ID)
    summary_card("Total Customers", format(total_customers, big.mark = ","))
  })
  
  output$average_age_card <- renderUI({
    df <- filtered_data()
    average_age <- mean(df$Age, na.rm = TRUE)
    summary_card("Average Age", round(average_age, 1))
  })
  
  output$total_sales_card <- renderUI({
    df <- filtered_data()
    total_sales <- sum(df$MntWines + df$MntFruits + df$MntMeatProducts + df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds, na.rm = TRUE) / 1000
    summary_card("Total Sales", paste0("$", format(total_sales, big.mark = ",", nsmall = 1), "k"))
  })
  
  output$total_countries_card <- renderUI({
    df <- filtered_data()
    total_countries <- n_distinct(df$Country)
    summary_card("Total Countries", format(total_countries, big.mark = ","))
  })
  
  output$education_distribution <- renderPlotly({
    df <- filtered_data()
    education_counts <- df %>% count(Education)
    
    fig <- plot_ly(education_counts, labels = ~Education, values = ~n, type = 'pie',
                   textposition = 'inside', textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                   text = ~paste(n, ' customers'), marker = list(colors = color_palette,
                                                                 line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
      layout(title = 'Education Level Distribution')
    fig
  })
  
  output$average_age_plot <- renderPlotly({
    df <- filtered_data()
    
    age_groups <- df %>%
      group_by(Age_Group) %>%
      summarise(avg_age = mean(Age))
    
    fig <- plot_ly(age_groups, labels = ~Age_Group, values = ~avg_age, type = 'pie',
                   textposition = 'inside', textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                   text = ~paste(round(avg_age, 1), ' years'), marker = list(colors = color_palette,
                                                                             line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
      layout(title = 'Average Age Distribution')
    fig
  })
  
  output$marital_kidhome_bar_chart <- renderPlotly({
    df <- filtered_data()
    
    marital_status_counts <- df %>% count(Marital_Status, Kidhome) %>%
      filter(Marital_Status != "YOLO")
    
    gg <- ggplot(marital_status_counts, aes(x = n, y = Marital_Status, fill = factor(Kidhome))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Count", y = "Marital Status", fill = "Number of Kids") +
      theme_minimal() +
      scale_fill_manual(values = color_palette) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14))
    
    ggplotly(gg)
  })
  
  output$spending_analysis_plot <- renderPlotly({
    df <- filtered_data()
    
    spending_columns <- c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")
    spending_data <- df %>% 
      summarise(across(all_of(spending_columns), sum, na.rm = TRUE)) %>%
      gather(key = "Category", value = "Amount") %>%
      mutate(Amount = Amount / 1000)  # Display amounts in thousands
    
    gg <- ggplot(spending_data, aes(x = Amount, y = Category, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(x = "Total Amount Spent (in thousands)", y = "Product Category") +
      theme_minimal() +
      scale_fill_manual(values = color_palette) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14))
    
    ggplotly(gg)
  })
  
  output$purchase_methods_plot <- renderPlotly({
    df <- filtered_data()
    
    purchase_columns <- c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth")
    purchase_data <- df %>% 
      summarise(across(all_of(purchase_columns), sum, na.rm = TRUE)) %>%
      gather(key = "Method", value = "Count")
    
    gg <- ggplot(purchase_data, aes(x = Count, y = Method, fill = Method)) +
      geom_bar(stat = "identity") +
      labs(x = "Total Count", y = "Purchase Method") +
      theme_minimal() +
      scale_fill_manual(values = color_palette) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14))
    
    ggplotly(gg)
  })
  
  output$sales_trend_plot <- renderPlotly({
    df <- filtered_data()
    
    sales_trend <- df %>%
      group_by(Dt_Customer) %>%
      summarise(total_products = sum(MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds, na.rm = TRUE) / 1000)  # Display amounts in thousands
    
    fig <- ggplot(sales_trend, aes(x = Dt_Customer, y = total_products)) +
      geom_area(fill = 'rgba(0,123,255,0.3)', color = 'rgba(0,123,255,1)', size = 0.3) +
      labs(title = 'Sales Trend Over Time', x = 'Date', y = 'Total Products (in thousands)') +
      theme_minimal() +
      theme(plot.title = element_text(hjust =0.5))
    ggplotly(fig) %>%
      layout(hovermode = "x unified")
  })
  
  output$customer_growth_plot <- renderPlotly({
    df <- filtered_data()
    
    customer_growth <- df %>%
      group_by(Dt_Customer) %>%
      summarise(new_customers = n())
    
    fig <- ggplot(customer_growth, aes(x = Dt_Customer, y = new_customers)) +
      geom_area(fill = 'rgba(0,74,134,0.3)', color = 'rgba(0,74,134,1)', size = 0.3) +
      labs(title = 'Customer Growth Over Time', x = 'Date', y = 'New Customers') +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(fig) %>%
      layout(hovermode = "x unified")
  })
  
  output$country_map <- renderLeaflet({
    df <- filtered_data()
    
    country_counts <- df %>% 
      count(Country) %>%
      mutate(Country = tolower(Country))
    
    # Get world map data
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world <- world %>% mutate(name_long = tolower(name_long))
    
    # Join country counts with map data
    world <- world %>% left_join(country_counts, by = c("name_long" = "Country"))
    world$values <- world$n
    world$values[is.na(world$values)] <- 0
    
    
    blue_palette <- colorNumeric(palette = c("#E0F7FA", "#039BE5", "#01579B"), domain = world$values)
    
    leaflet(world) %>%
      addPolygons(fillColor = ~blue_palette(values),
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = ~paste0("<strong>Country: </strong>", name_long, "<br><strong>Count: </strong>", values),
                  highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
                  label = ~paste0(name_long, ": ", values),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = blue_palette, values = ~values, opacity = 0.8, title = "Number of Customers",
                position = "bottomright")
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data(), 
              options = list(
                pageLength = 10, 
                searchHighlight = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'print'),
                className = 'stripe hover',
                style = 'bootstrap',
                rownames = FALSE,
                filter = 'top'
              ),
              extensions = 'Buttons',
              class = 'cell-border stripe')
  })
  
  observeEvent(input$send_email, {
    
    email <- compose_email(
      body = md("## Data Update Notification\n\nThe data has been updated successfully."),
      footer = md("This is an automated message from your Shiny app.")
    )
    
    smtp_send(
      email,
      from = "darka805@example.com",
      to = "darka805@example.com",
      subject = "Data Update Notification",
      credentials = creds_file("smtp_creds")
    )
    
    output$email_status <- renderText("Email sent successfully.")
  })
}

shinyApp(ui = ui, server = server)
                                        
      
