library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Air Temperature"),
  dashboardSidebar(
    collapsed = TRUE,
    menuItem("Temperature", tabName = "temp_tab", icon = icon("thermometer"))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "temp_tab",
        fluidPage(
          titlePanel("Temperature Dashboard"),
          sidebarLayout(
            sidebarPanel(
              # Add any input controls if needed
            ),
            mainPanel(
              plotOutput("temperaturePlot")
            )
          )
        )
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  # Your existing code to fetch and process data
  res <- GET(paste("https://api.data.gov.sg/v1/environment/air-temperature?date=",Sys.Date(),sep = ""))
  data <- fromJSON(rawToChar(res$content))
  
  # Convert data to a dataframe
  ## Clean the timestamp
  ### Remove the last six character (i.e  +08:00)
  data$items$timestamp <- substr(data$items$timestamp,1,nchar(data$items$timestamp)-6)
  ### Convert T to whitespace
  data$items$timestamp <- gsub("T", " ",data$items$timestamp)
  ## Set name for index
  data_list <- setNames(data$items$readings, data$items$timestamp)
  ## Convert lists to dataframes
  final_data <- do.call(rbind, data_list)
  
  ## Convert index to a column name - DATETIME
  final_data$DATETIME <- rownames(final_data)
  ## Remove row name
  rownames(final_data) <- NULL
  ## Clean up the timestamp
  final_data$DATETIME <- gsub("\\..*","",final_data$DATETIME)
  ## Convert timestamp from char to datetime
  final_data$DATETIME <- as.POSIXct(final_data$DATETIME)
  
  # Choose required columns in metadata-station
  station_data <- data$metadata$stations %>%
    select(id,name)
  # Link station data to final_data
  final_weather_data <- final_data %>%
    left_join(station_data, by = c('station_id'='id')) %>%
    select(DATETIME,name,value)
  
  # Assuming you have a column 'value' in final_weather_data
  output$temperaturePlot <- renderPlot({
    ggplot(final_weather_data, aes(x = DATETIME, y = value, color = name)) +
      geom_line() +
      labs(title = "Temperature Over Time",
           x = "Datetime",
           y = "Temperature (Celsius)")
  })
})

shinyApp(ui, server)
