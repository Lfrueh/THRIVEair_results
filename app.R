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
library(plotly)
library(shinydashboard)

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
    titlePanel("VOC Concentrations at Different Sites"),
    sidebarLayout(
        sidebarPanel(
            selectInput("pollutant", "Select Pollutant:", choices = voc, selected = default_pollutant),
            selectInput("date_filter", "Select Date:", choices = c("Month Average", unique(data$end_date)), selected = default_date_filter),
            plotlyOutput("line_chart") # Add plotly output for the line chart
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
        
        voc_name <- names(voc[voc == input$pollutant])
        
        # Create a custom title for the map
        title_html <- paste("<h4>Pollutant: ", voc_name, "</h4>",
                            "<h4>Date: ", input$date_filter, "</h4>")
        
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
                ) %>%
            addControl(
                html = title_html,
                position = "topright"
            )
            
    })
    
    # Render the line chart
    output$line_chart <- renderPlotly({
        # Filter data for the line chart based on input$date_filter
        #chart_data <- filtered_data() 
        #month_dat <- data %>% group_by(end_date) %>% mutate(across(benzene:btex, mean))
        
        if (input$date_filter == "Month Average") {
            avg_data <- data %>%
                group_by(site_id) %>%
                mutate(across(benzene:btex, mean)) %>%
                ungroup() %>%
                select(-begin_date, -end_date, -week_num) %>%
                distinct() %>%
                arrange(.data[[input$pollutant]]) # Arrange by the selected pollutant
            
            pal <- colorNumeric(palette = "BuPu", domain = avg_data[[input$pollutant]])
            
            # Reorder the site levels based on the ascending values of the selected pollutant
            avg_data$site <- factor(avg_data$site, levels = avg_data$site[order(avg_data[[input$pollutant]])])
            
            # Get the cleaner name for the selected pollutant from the 'voc' vector
            voc_name <- names(voc[voc == input$pollutant])
        
            plot_ly(data = avg_data, x = ~site, y = ~get(input$pollutant), 
                    color = ~site, type = "scatter", mode = "lines+markers",
                    marker = list(color = ~pal(avg_data[[input$pollutant]])), 
                    showlegend=FALSE, text = ~site, hoverinfo = 'text') %>%
                layout(title = paste("Monthly Average:", "<br>",voc_name),
                       xaxis = list(title = "Site"),
                       yaxis = list(title = voc_name)) 
        } else {
            chart_data <- filtered_data() %>%
                arrange(.[[input$pollutant]]) # Arrange by the selected pollutant
            
            # Reorder the site levels based on the ascending values of the selected pollutant
            chart_data$site <- factor(chart_data$site, levels = chart_data$site[order(chart_data[[input$pollutant]])])
            
            # Get the cleaner name for the selected pollutant from the 'voc' vector
            voc_name <- names(voc[voc == input$pollutant])
            
            #Color palette
            pal <- colorNumeric(palette = "BuPu", domain = chart_data[[input$pollutant]])
            
            plot_ly(data = chart_data, x = ~site, y = ~get(input$pollutant), 
                    color = ~site, type = "scatter", mode = "lines+markers", 
                    marker = list(color = ~pal(chart_data[[input$pollutant]])), 
                    showlegend=FALSE,
                    mode = text,
                    #text = ~site,
                    text = paste(chart_data$site,":", voc_name),
                    #textposition = 'middle right',
                    hoverinfo = 'text') %>%
                #add_text(textposition = 'middle right') %>%
                layout(title = paste(voc_name, ":", "<br>", input$date_filter),
                       xaxis = list(title = "Site"),
                       yaxis = list(title = voc_name)) 
        }
    }) 
}


    



# Run the application 
shinyApp(ui = ui, server = server)
