library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(sf)
library(lubridate)

#NOTES:
# Eventually, we will need to add a 'month' variable so that the 'month average' selection is also contingent 
# on whether someone selected a particular month. So the decision structure would be like:
# Select time frame: slider???
# Select pollutant
# Select specific date or a month average or a running average


library(mapboxapi)
mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
mb_access_token(mapbox_token, install = TRUE, overwrite = TRUE)


# Data Cleaning ----

# Group by site and calculate site averages (within-site, across month) 
#and date average (within-date, across sites) for each pollutant

#Let's average co-lo's for now, and convert to micrograms per meter cubed
data <- read_csv("dat.csv", ) %>%
    group_by(end_date, site_id) %>%
    mutate(across(benzene:btex, mean)) %>%
    distinct() %>%
    mutate(
        begin_date = as.Date(begin_date, format = "%m/%d/%y"),
        end_date = as.Date(end_date, format = "%m/%d/%y"),
        benzene = benzene*78.11/24.45,
        toluene = toluene*92.14/24.45,
        etbenz = etbenz*106.167/24.45,
        xylenes = xylenes*106.16/24.45,
        btex = btex*594.91/24.45
    )

#refinery
# refinery <- st_read("refinery/Refinery.shp") %>%
#     st_transform(., crs = 4326)


voc <- c(
    "Benzene" = "benzene",
    "Toluene" = "toluene",
    "Ethylbenzene" = "etbenz",
    "Xylenes" = "xylenes",
    "Total BTEX" = "btex"
)


# User Interface ----

# Define UI for application that draws a map
ui <- fluidPage(
    titlePanel("VOC Concentrations at Different Sites"),
    sidebarLayout(
        sidebarPanel(
            selectInput("pollutant", "Select Pollutant:", choices = voc, selected = "btex"),
            sliderInput("date_range", "Select Date Range:", value = c(min(data$end_date), max(data$end_date)), min = min(data$end_date), max = max(data$end_date)),
            selectInput("date_filter", "Select Date:", choices = NULL, selected = "Average Across Date Range"),
            ),
        mainPanel(
            plotlyOutput("map"),
            plotlyOutput("bar_chart")
        )
    )
)

#tiny change

#  Server ----

server <- function(input, output, session) {
    
    data_range <- reactive({
        selected_range <- input$date_range
        filter(data, end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
    })
    
    observe({
        date_choices <- c("Average Across Date Range", format(unique(data_range()$end_date), "%m/%d/%y"))
        updateSelectInput(session, "date_filter", choices = date_choices)
    })
    
    
    # Filter data based on user input
    filtered_data <- reactive({
        if (input$date_filter == "Average Across Date Range") {
            # Calculate monthly averages for each pollutant
            avg <- data_range() %>%
                group_by(site_id) %>%
                mutate(across(benzene:btex, mean)) %>%
                ungroup() %>%
                select(-begin_date, -end_date, -week_num) %>%
                distinct()
            return(avg)
        } else {
            selected_date <- as.Date(input$date_filter, "%m/%d/%y")
            return(filter(data_range(), end_date == selected_date))
        }
    })
    
    
    
## Render Map ----
    output$map <- renderPlotly({
        # Clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        
        # Create a custom title for the map
        title_html <- paste(voc_name, ": ", input$date_filter)
        
        #standardize color scale max across time filters
        cmax <- max(data[[input$pollutant]])
        cmin <- 0.9*min(data[[input$pollutant]])
        
           plot_mapbox(filtered_data()) %>%
               add_trace(
                   type = "scattermapbox",
                   mode = "markers",
                   lat = ~lat,
                   lon = ~long,
                   marker = list(
                       size = 21,
                       color = "black",
                       opacity = 1.0
                   ),
                   hoverinfo = "none"
               ) %>%
            add_trace(
                type = "scattermapbox",
                mode = "markers",
                lat = ~lat,
                lon = ~long,
                marker = list(
                    size = 20,
                    color = filtered_data()[[input$pollutant]],
                    opacity = 1.0,
                    cmin = cmin,
                    cmax = cmax,
                    colorbar = list(
                        title = paste0(voc_name, "<br>"," (", HTML("&mu;"), "g/m³",")")
                    ),
                    colorscale = "Viridis",
                    reversescale = TRUE
                ),
                text = ~paste(site),
                hoverinfo = "text",
                showlegend = FALSE
            ) %>%
               layout(
                   mapbox = list(
                   layers = list(
                       below = ""
                       ),
                   style = "streets",
                   center = list(lat = mean(data$lat), lon = mean(data$long)),
                   zoom = 12),
                   title = title_html
               )
           # %>%
           #  add_sf(
           #      data = refinery,
           #      type = "scattermapbox",
           #      inherit = FALSE,
           #      fillcolor = 'rgba(211,211,211,0.5)',
           #      opacity = 0.2,
           #      line = list(color = "grey", width = 0.2),
           #      text = "Refinery",
           #      hoverinfo = "text",
           #      below = "markers")
            
    })
    

    
## Render Bar Chart ----
    output$bar_chart <- renderPlotly({
        #clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        
        #standardize color scale max across time filters
        cmax <- max(data[[input$pollutant]])
        cmin <- 0.9*min(data[[input$pollutant]])
        
        if (input$date_filter == "Month Average") {
            avg_data <- data %>%
                group_by(site_id) %>%
                mutate(across(benzene:btex, mean)) %>%
                ungroup() %>%
                select(-begin_date, -end_date, -week_num) %>%
                distinct()

            plot_ly(data = avg_data, x = ~site, y = avg_data[[input$pollutant]], 
                    type = "bar", 
                    marker = list(
                        color = ~avg_data[[input$pollutant]], 
                        colorscale = "Viridis",
                        reversescale = TRUE,
                        cmin = cmin,
                        cmax = cmax), 
                    showlegend=FALSE, text = ~site, hoverinfo = 'text') %>%
                layout(title = paste("Monthly Average:", "<br>",voc_name),
                       xaxis = list(
                           title = "Site",
                           showticklabels = FALSE,
                           categoryorder = "total ascending"),
                       yaxis = list(
                           title = paste0(voc_name, " (", HTML("&mu;"), "g/m³",")"))
                )
        } else {
            chart_data <- filtered_data()
            
            plot_ly(data = chart_data, x = ~site, y = chart_data[[input$pollutant]], 
                    type = "bar", 
                    marker = list(
                        color = ~chart_data[[input$pollutant]], 
                        colorscale = "Viridis",
                        reversescale = TRUE,
                        cmin = cmin,
                        cmax = cmax), 
                    showlegend=FALSE, text = ~site, hoverinfo = 'text') %>%
                layout(title = paste(voc_name, ":", "<br>", input$date_filter),
                       xaxis = list(
                           title = "Site",
                           showticklabels = FALSE,
                           categoryorder = "total ascending"),
                       yaxis = list(
                           title = paste0(voc_name, " (", HTML("&mu;"), "g/m³",")"))
                       ) 
        }
    }) 
}


    



# Run the application 
shinyApp(ui = ui, server = server)
