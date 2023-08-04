#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(rgdal)

# Group by site and calculate site averages (within-site, across month) 
#and date average (within-date, across sites) for each pollutant



#Let's average co-lo's for now
data <- read_csv("dat_june.csv") %>%
    group_by(end_date, site_id) %>%
    mutate(across(benzene:btex, mean)) %>%
    distinct()

#refinery
refinery <- readLines("refinery.geojson") %>% paste(collapse = "\n") 



voc <- c(
    "Benzene" = "benzene",
    "Toluene" = "toluene",
    "Ethylbenzene" = "etbenz",
    "Xylenes" = "xylenes",
    "Total BTEX" = "btex"
)


default_pollutant <- "btex"
default_date_filter <- "Month Average"

# Define UI for application that draws a map
ui <- fluidPage(
    titlePanel("Pollutant Concentrations at Different Sites"),
    sidebarLayout(
        sidebarPanel(
            selectInput("pollutant", "Select Pollutant:", choices = voc, selected = default_pollutant),
            selectInput("date_filter", "Select Date:", choices = c("Month Average", unique(data$end_date)), selected = default_date_filter)
        ),
        mainPanel(
            leafletOutput("map")
        )
    )
)

server <- function(input, output, session) {
    # Filter data based on user input
    filtered_data <- reactive({
        if (input$date_filter == "Month Average") {
            # Calculate monthly averages for each pollutant
            monthly_avg <- data %>%
                group_by(site_id) %>%
                mutate(across(benzene:btex, mean)) %>%
                ungroup() %>%
                select(-begin_date, -end_date, -week_num) %>%
                distinct()
            return(monthly_avg)
        } else {
            return(data %>% filter(end_date == input$date_filter))
        }
    })
    

    
    # Render the map
    output$map <- renderLeaflet({
        # Color palette for the circle markers
        pal <- colorNumeric(palette = "BuPu", domain = filtered_data()[[input$pollutant]]) 
        
        leaflet() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            addGeoJSON(refinery,
                       color="grey",
                       weight = 1) %>%
            setView(lng = mean(data$long), lat = mean(data$lat), zoom = 13) %>%
            clearMarkers() %>%
            addCircleMarkers(
                data = filtered_data(),
                lng = ~long,
                lat = ~lat,
                weight = 1,
                opacity = 0.5,
                fillOpacity = 1,
                fillColor = ~pal(filtered_data()[[input$pollutant]]), 
                color = "black", 
                #radius = ~sqrt(get(input$pollutant)) * 20,
                popup = ~paste("Site: ", site, "<br>")
                               #,
                               #"Concentration: ", round(get(input$pollutant),2)
                )
            
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
