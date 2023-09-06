library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(sf)
library(lubridate)
library(DT)
library(shinyWidgets)
library(fresh)




library(mapboxapi)
mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
mb_access_token(mapbox_token, install = TRUE, overwrite = TRUE)

my_theme = create_theme(
  adminlte_color(
    light_blue = "#FFF"
  ),
  adminlte_sidebar(
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#3ace88",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    info_box_bg = "#D8DEE9"
  )
)




# Data Cleaning ----

# Group by site and calculate site averages (within-site, across month) 
#and date average (within-date, across sites) for each pollutant

#Let's average co-lo's for now, and convert to micrograms per meter cubed
data <- read_csv("dat.csv") %>%
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
    ) %>%
    ungroup()

voc <- c(
    "Benzene" = "benzene",
    "Toluene" = "toluene",
    "Ethylbenzene" = "etbenz",
    "Xylenes" = "xylenes",
    "Total BTEX" = "btex"
)

other_vars <- c(
    "Site" = "site",
    "Sample begin date" = "begin_date",
    "Sample end date" = "end_date",
    "Latitude" = "lat",
    "Longitude" = "long"
)

units <- c(
    "µg/m³",
    "ppbv"
)

end_dates <- unique(data$end_date)


# User Interface ----

# User Interface ----

ui <- dashboardPage(
    dashboardHeader(
      title = "THRIVEair Results",
      tags$li(a(href = "https://thriveairphilly.com/", img(src = "logo.png", height = "30px")), class = "dropdown")
                    ),
    dashboardSidebar(
        sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                    menuItem("Information", tabName = "docs", icon = icon("circle-info"))
                    
        ),
        tags$script(HTML('
    $(document).ready(function() {
      // Set a static tooltip text
      $(".sidebar-toggle").attr("data-tooltip", "Toggle Navigation Pane");
    });
  '))
    ),
    dashboardBody(
      use_theme(my_theme),
      includeCSS("style.css"),
        # Content for the "Dashboard" tab
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                      box(
                        title = "Instructions",
                  #      status = "info",  
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 12,
                        uiOutput("instruction_text")  
                      )
                    ),
                    
                    fluidRow(
                        box(
                            selectInput("pollutant", "Select Pollutant:", choices = voc, selected = "btex"),
                            
                            sliderTextInput(
                                inputId = "date_range", label = "Select Monitoring End Dates:", 
                                choices = end_dates, selected = range(end_dates), 
                                grid = TRUE
                            ),

                            selectInput("date_filter", "Select Date:", choices = NULL, selected = "Average Across Date Range"),
                            width = 6, title = "Select Data",
                            collapsible = TRUE,
                            solidHeader = TRUE,
                        ),
                        box(
                            uiOutput("info"),
                            downloadButton("download", "Download Filtered Data as .csv"),
                            width = 6, title = "Data Summary",
                            collapsible = TRUE,
                            solidHeader = TRUE,
                        )
                    ),
                    fluidRow(
                        box(plotlyOutput("map"), width = 100, collapsible = TRUE, solidHeader = TRUE, title = "Map")),
                    fluidRow(
                        box(plotlyOutput("bar_chart"), width = 100, collapsible = TRUE, solidHeader = TRUE, title = "Bar Chart")),
                    fluidRow(
                        box(plotlyOutput("line_chart"), width = 100, collapsible = TRUE, solidHeader = TRUE, title = "Concentrations Over Time")
                    )
            ),
            # Content for the "Data Table" tab
             tabItem(tabName = "data_table",
                     fluidRow(
                         column(6,
                                checkboxGroupInput("col_voc", "Select Pollutant(s):",
                                                   voc, selected = voc),
                                radioButtons("units", "Select Units",
                                                   units, selected = units[1])),
                         column(6, checkboxGroupInput("col_vars", "Select Other Variables:",
                                                      other_vars, selected = other_vars),
                                sliderTextInput(
                                    inputId = "date_range2", label = "Select Monitoring End Dates:", 
                                    choices = end_dates, selected = range(end_dates), 
                                    grid = TRUE
                                )
                             
                     )),
                     fluidRow(
                         box(
                             dataTableOutput("table"), 
                             style = "overflow-x: scroll",width = 100)
                         )
                     
             ),
            tabItem(tabName = "docs",
                    fluidRow(
                      box(
                        uiOutput("documentation"),
                        width = 12
                      )
                    )
              
            )
         )
)
)



#  Server ----

server <- function(input, output, session) {
    
    
    output$instruction_text <- renderUI({
      HTML("<span style='color:black;'> 
            Select data using the menu options below. 
            To zoom, select, or download a photo of a plot or graph, hover over the plot until the controls appear on the right-hand side.
            Hover over each button to learn its function.
            To collapse these instructions or any section on this page, click the '--' symbol in the upper right-hand corner of the content box.
            <br><br>
            To see and download a data table, and for more information, click on the 'Data Table' or 'Information' tabs in the left sidebar.
           </span>")
    })
    
    
    output$documentation <- renderUI({
      HTML(paste("<span> \
      <h3> About THRIVEair </h3>
          THRIVEair is a community-based air monitoring project in South and Southwest Philadelphia.
          Designed as a partnership between <a href='https://www.phillythrive.org/'> Philly Thrive </a>
          and researchers at the <a href= 'https://drexel.edu/dornsife/'> Drexel Dornsife School of Public Health</a>, 
          data will be collected from summer 2023 - summer 2024.
          <br> <br>
         <strong> For more information on the study, visit our website at: <a href = 'https://thriveairphilly.com/'>THRIVEairPhilly.com </a> </strong>
      <br> <br>
      <h3> Interpreting These Data </h3>
           Volatile organic compounds (VOCs) were monitored in one-week sessions, 
           with begin dates representing the date of monitor deployment and end dates representing the date of data retrieval.
           Concentrations shown in these results are one week averages over the sampling period.
           <br><br>
           For example, a benzene concentration of 1 µg/m³ on 6/14/23 is interpreted as 
           the <strong>one-week average concentration</strong> at that site for the week beginning on 6/7 and ending on 6/14.
           <br> <br>
      <h3> Contact Us </h3>
           
           <strong>E-mail:</strong> THRIVEair@gmail.com
           <br>
           <strong>Other:</strong> <a href= 'https://thriveairphilly.com/contact/'> Contact Form </a>

           
           
           </span>"))
    })
    

## Update date selections ----
    data_range <- reactive({
        selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
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
              selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
                min_date <- selected_range[1]
                max_date <- selected_range[2]
                # Set the filename for the downloaded CSV
                paste("THRIVEair_results_", min_date, "_to_", max_date, ".csv", sep = "")
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
                   title = title_html) %>%
               config(scrollZoom = FALSE)
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
                ) %>%
              config(scrollZoom = FALSE)
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
                       ) %>%
              config(scrollZoom = FALSE)
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
        
        data_range() %>% 
            ungroup() %>%
            plot_ly() %>%
            add_trace(
                split = ~site,
                x = ~end_date,
                y = ~get(input$pollutant),
                type = "scatter",
                mode = "markers + lines",
                name = ~site,
                connectgaps = TRUE,
                text = paste0(data_range()$site,", ",data_range()$end_date), hoverinfo = 'text')%>% 
            layout(
            title = "Concentrations over time by site",
            xaxis = list(title = "Date"),
            yaxis = list(title = paste0(voc_name, " (", HTML("&mu;"), "g/m³", ")")),
            showlegend = TRUE
        ) %>%
          config(scrollZoom = FALSE)

    })
    
    
    table_data <- reactive({
        
        mws <- c(
            "benzene" = 78.11,
            "toluene" = 92.14,
            "etbenz" = 106.167,
            "xylenes" = 106.16,
            "btex" = 594.91
        )
        

        selected_range <- as.Date(input$date_range2, format = "%Y-%m-%d")
        filtered <- filter(data, end_date %in% seq(selected_range[1], selected_range[2], by = "day")) %>%
            select(all_of(c(input$col_vars, input$col_voc))) %>%
            mutate(across(input$col_voc, round, 3))

        if (input$units == "µg/m³"){
            filtered <- filtered
        } 
        if (input$units == "ppbv") {
            for (pollutant in names(mws)){
                if (pollutant %in% colnames(filtered)){
                    filtered <- filtered %>%
                        mutate(!!pollutant := ifelse(!is.na(!!sym(pollutant)), 
                                                     round(!!sym(pollutant) / mws[pollutant] * 24.45, 3), 
                                                     !!sym(pollutant)))
                }
            }
        }



        
        return(filtered)
    })
    
    output$table <- renderDataTable({
        datatable(table_data(),
                  extensions = 'Buttons', 
                  options = list(scrollX=TRUE, 
                                 paging = TRUE, 
                                 searching = TRUE,
                                # fixedColumns = TRUE, 
                                # autoWidth = TRUE,
                                 ordering = TRUE, 
                                dom = 'Bfrtip',
                                 buttons = list(list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                 ))))
    })
    
}


    



# Run the application 
shinyApp(ui = ui, server = server)






