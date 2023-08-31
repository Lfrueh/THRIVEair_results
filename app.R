library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(sf)
library(lubridate)


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
ui <- fluidPage(
    titlePanel("VOC Concentrations at Different Sites"),
    
    tabsetPanel(
        tabPanel("Plots", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("pollutant", "Select Pollutant:", choices = voc, selected = "btex"),
                         sliderInput("date_range", "Select Date Range:", value = c(min(data$end_date), max(data$end_date)), min = min(data$end_date), max = max(data$end_date),
                                     timeFormat = "%m/%d/%y"),
                         selectInput("date_filter", "Select Date:", choices = NULL, selected = "Average Across Date Range"),
                         htmlOutput("info"),
                         downloadButton("download", "Download Filtered Data as .csv")
                     ),
                     mainPanel(
                         plotlyOutput("map"),
                         plotlyOutput("bar_chart"),
                         plotlyOutput("line_chart")
                     )
                 )
                 ),
        tabPanel("Data Table", fluid = TRUE,
                 fillPage(dataTableOutput("table"))
                 # sidebarLayout(
                 #     sidebarPanel(
                 #         # selectInput("pollutant", "Select Pollutant:", choices = voc, selected = "btex"),
                 #         # sliderInput("date_range", "Select Date Range:", value = c(min(data$end_date), max(data$end_date)), min = min(data$end_date), max = max(data$end_date),
                 #         #             timeFormat = "%m/%d/%y"),
                 #         # selectInput("date_filter", "Select Date:", choices = NULL, selected = "Average Across Date Range"),
                 #         # downloadButton("download", "Download Data as .csv")
                 #     ),
                 #     mainPanel(
                 #         dataTableOutput("table")
                 #     )
                 # )
                 )
    )

)


#  Server ----

server <- function(input, output, session) {

## Update date selections ----
    data_range <- reactive({
        selected_range <- input$date_range
        filter(data, end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
    })
    
    observe({
        date_choices <- c("Average Across Date Range", format(unique(data_range()$end_date), "%m/%d/%y"))
        updateSelectInput(session, "date_filter", choices = date_choices)
    })

## Filter data ----
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
    
        
## Render reactive text ----
    observe({
        # Clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        # Calculate the average concentration for the selected range
        avg_selected_range <- round(mean(filtered_data()[[input$pollutant]]),2)
        # Calculate the study-wide average concentration for the selected pollutant
        avg_study_wide <- round(mean(data[[input$pollutant]]),2)
        # Create a conditional expression comparing the selected pollutant to the study-wide average
        comparison <- if (is.na(avg_selected_range) || is.na(avg_study_wide)) {
            "N/A"
        } else if (avg_selected_range > avg_study_wide) {
            "<span style='color:red;font-weight:bold;'>higher than </span>"
        } else if (avg_selected_range < avg_study_wide) {
            "<span style='color:green;font-weight:bold;'>lower than </span>"
        } else {
            "<span style='color:black;'>the same as </span>"
        }
        # Create the reactive text
        output$info<- renderUI({
            if (input$date_filter == "Average Across Date Range") {
                HTML(paste("The average concentration of", voc_name, 
                           "for the selected date range was <strong>", round(avg_selected_range, 2), ("&mu;"), "g/m³",
                           "</strong>.", "This is", comparison, "the study-wide average of", 
                           "<strong>", round(avg_study_wide, 2), ("&mu;"), "g/m³","</strong>."))
            } else {
            HTML(paste("The average concentration of", voc_name, 
                  "for the week ending on", input$date_filter, "was <strong>", round(avg_selected_range, 2), ("&mu;"), "g/m³",
                  "</strong>.", "This is", comparison, "the study-wide average of", 
                  "<strong>", round(avg_study_wide, 2), ("&mu;"), "g/m³","</strong>."))
            }
        })

## Download button ----
        output$download <- downloadHandler(
            filename = function() {
                # Get the minimum and maximum dates from the selected range
                min_date <- format(input$date_range[1], "%Y-%m-%d")
                max_date <- format(input$date_range[2], "%Y-%m-%d")
                # Set the filename for the downloaded CSV
                paste("filtered_data_", min_date, "_to_", max_date, ".csv", sep = "")
            },
            content = function(file) {
                # Write the filtered data to a CSV file
                write.csv(filtered_data(), file)
            }
        )
        
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
    })
    

    
## Render Bar Chart ----
    output$bar_chart <- renderPlotly({
        #clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        
        #standardize color scale max across time filters
        cmax <- max(data[[input$pollutant]])
        cmin <- 0.9*min(data[[input$pollutant]])
        
        if (input$date_filter == "Average Across Date Range") {
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
                layout(title = paste("Comparing Average Concentrations of", "<br>",voc_name, "in the", "<br>","Selected Date Range Between Sites"),
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
                layout(title = paste("Comparing Concentrations of",voc_name, "on", "<br>", input$date_filter, "Between Sites"),
                       xaxis = list(
                           title = "Site",
                           showticklabels = FALSE,
                           categoryorder = "total ascending"),
                       yaxis = list(
                           title = paste0(voc_name, " (", HTML("&mu;"), "g/m³",")"))
                       ) 
        }
    }) 
    
## Render Line Chart ----
    output$line_chart <- renderPlotly({
        # Clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        # Create a title for the line chart
        line_title <- paste(voc_name, "Concentration Over Time")
        # Get unique site list
        sites <- unique(data_range()$site)
        
        p <- plot_ly()
        
        for (site_i in sites) {
            site_data <- data_range() %>%
                filter(site == site_i) %>% 
                ungroup()
            
            p <- p %>%
                add_trace(data = site_data,
                          x = ~end_date,
                          y = ~get(input$pollutant),
                          type = "scatter",
                          mode = "markers + lines",
                          name = site_i,
                          connectgaps = TRUE)
        }
        
        p <- p %>% layout(
            title = "Concentrations over time by site",
            xaxis = list(title = "Date"),
            yaxis = list(title = paste0(voc_name, " (", HTML("&mu;"), "g/m³", ")")),
            showlegend = TRUE
        )
        
        p

    })
    
    output$table <- renderDataTable({
        data
    })
    
}


    



# Run the application 
shinyApp(ui = ui, server = server)






