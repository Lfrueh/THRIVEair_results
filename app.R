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
library(readxl)

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

data <- read_csv("data/clean/dat.csv") %>%
  filter(site_type == "stationary") %>%
    mutate(
        start_date = as.Date(start_date, format = "%m/%d/%y"),
        end_date = as.Date(end_date, format = "%m/%d/%y")
    ) %>%
  ungroup() %>%
  select(-...1) %>%
  select(-ends_with("flag"), -site_type) %>%
  arrange(., end_date) 

#Hilco benzene data, already in micrograms per meter cubed
hilco <- read_csv("data/clean/hilco.csv") %>%
  select(-...1) %>%
  arrange(., end_date) %>%
  mutate(benzene = as.numeric(benzene))


codebook <- read_excel("data/codebook.xlsx")
voc <- setNames(codebook$variable_name, codebook$voc_name)
mws <- setNames(codebook$mw, codebook$variable_name)


#Convert to microgram per meter cubed
for (pollutant in names(mws)){
  if (pollutant != "btex") {
    data <- data %>%
      mutate(!!pollutant := ifelse(!is.na(!!sym(pollutant)), 
                                   round(!!sym(pollutant) * mws[pollutant] / 24.45, 3), 
                                   !!sym(pollutant)))
  }
}

data <- data %>%
  mutate(btex = rowSums(select(data, benzene:xylenes), na.rm = TRUE)) %>%
  mutate(btex = round(btex, 3)) %>%
  relocate(btex, .after = "xylenes")

other_vars <- c(
    "Site" = "site",
    "Sample begin date" = "start_date",
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
        sidebarMenu(menuItem("Dashboard", icon = icon("dashboard"),
                             menuSubItem(HTML("THRIVEair Data:<br>All Pollutants"), tabName = "dashboard", icon = icon("dashboard"), selected = TRUE), 
                             menuSubItem(HTML("THRIVEair +<br>Bellwether District:<br>Benzene Data"), tabName = "hilco", icon = icon("location-dot"))
                             ),
                    menuItem("Compare Pollutants", tabName = "compare", icon=icon("code-compare")),
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
                            selectInput("pollutant", "Select Pollutant:", choices = voc, selected = "benzene"),
                            
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
                        box(plotlyOutput("map"), width = 100, collapsible = TRUE, solidHeader = TRUE, title = "Comparing Concentrations Across Sites")),
                    fluidRow(
                        box(plotlyOutput("bar_chart"), width = 100, collapsible = TRUE, solidHeader = TRUE, title = "Comparing Concentrations Across Sites")),
                    fluidRow(
                        box(plotlyOutput("line_chart"), width = 100, collapsible = TRUE, solidHeader = TRUE, title = "Comparing Concentrations Over Time")
                    )
            ),
            # Content for the "Data Table" tab
             tabItem(tabName = "data_table",
                     fluidRow(
                         column(6,
                                checkboxGroupInput("col_voc", "Select Pollutant(s):",
                                                   voc, selected = voc)),
                         column(6, checkboxGroupInput("col_vars", "Select Other Variables:",
                                                      other_vars, selected = other_vars),
                                sliderTextInput(
                                    inputId = "date_range2", label = "Select Monitoring End Dates:", 
                                    choices = end_dates, selected = range(end_dates), 
                                    grid = TRUE
                                ),
                                radioButtons("units", "Select Units",
                                             units, selected = units[1]),
                                radioButtons("flag", "Show Measurement Flags?",
                                             c("Yes", "No"), selected = "Yes"),
                                downloadButton("codebook","Download Codebook"),
                                HTML("<br><br>"),
                                box(uiOutput("table_info"), width = 100)
                             
                     )),
                     fluidRow(
                         box(
                             dataTableOutput("table"), 
                             style = "overflow-x: scroll",width = 100)
                         )
                     
             ),
            # Content for the Details tab
            tabItem(tabName = "docs",
                    fluidRow(img(src='combo_logo.png', height="50%", width = "50%", align = 'center'), width = 12
                    ),
                    fluidRow(
                      box(
                        uiOutput("documentation"),
                        width = 12
                      )
                    )
              
            ),
            # Content for the Comparison Tab
            tabItem(tabName = "compare",
                    fluidRow(
                      box(
                        selectInput("comp1", "Select Pollutant 1:", choices = voc, selected = "benzene"),
                        selectInput("comp2", "Select Pollutant 2:", choices = voc, selected = "toluene"),
                        selectInput("comp3", "Select Pollutant 3:", choices = voc, selected = "etbenz"),
                        selectInput("comp4", "Select Pollutant 4:", choices = voc, selected = "xylenes"),
                        width = 6, title = "Select Pollutants for Comparison",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                      ),
                      box(
                        sliderTextInput(
                          inputId = "date_range_comp", label = "Select Monitoring End Dates:", 
                          choices = end_dates, selected = range(end_dates), 
                          grid = TRUE
                        ),
                        width = 6, title = "Select Dates",
                        collapsible = TRUE,
                        solidHeader = TRUE,
                      )
                    ),
                    fluidRow(
                      box(plotlyOutput("compchart"), width = 100, height = 820),
                    
                    )),
            tabItem(tabName = "hilco",
                    fluidRow(
                      box(title = "What is this?",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          width = 12,
                          uiOutput("hilco_text"),
                          downloadButton("hilcodownload","Download Bellwether District Data"),
                          HTML("<em><br>To download THRIVEair data, visit the Data Table page.</em>")
                          )
                    ),
                    box(title = "Map", plotlyOutput("hilcomap"), width = 100, collapsible = TRUE),
                    box(title = "Bar Chart", plotlyOutput("hilcobarchart"), width = 100, collapsible = TRUE),   
                    box(title = "Changes Over Time", plotlyOutput("hilcotimeseries"), width = 100, collabsible = TRUE)
                    )
                    
         )
    )
)





#  Server ----

server <- function(input, output, session) {
  
  pal <- list(c(0, "lightpink"), c(1, "darkblue"))
#  pal <- "Viridis"
    
    
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
    
    output$hilco_text <- renderUI({
      HTML("<span style='color:black;'> 
            In February 2024, <a href = 'https://www.thebellwetherdistrict.com/'>The Bellwether District</a> added a benzene monitor on the former refinery site at 1st Street.
            Samples were collected using the same protocol as the rest of the THRIVEair study, but were analyzed at a different laboratory.
            Results are shown here for comparison.
           </span><br><br>")
    })
    
    output$documentation <- renderUI({
      HTML(paste("<span> \
      <h3> About THRIVEair </h3>
          THRIVEair is a community-based air monitoring project in South and Southwest Philadelphia.
          Designed as a partnership between <a href='https://www.phillythrive.org/'> Philly Thrive </a>
          and researchers at the <a href= 'https://drexel.edu/dornsife/'> Drexel Dornsife School of Public Health</a> and
          <a href = 'https://www.lcsc.edu/'>Lewis Clark State college</a>.
          Data will be collected from summer 2023 - summer 2024.
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
           For more details on sampling methodology and interpretation, download the codebook available in the Data Table tab.<br><br>
      <h3> Contact Us </h3>
           
           <strong>E-mail:</strong> THRIVEair@gmail.com
           <br>
           <strong>Other:</strong> <a href= 'https://thriveairphilly.com/contact/'> Contact Form </a>

           
           
           </span>"))
    })
    
    output$table_info <- renderUI({
      HTML(paste("<span> \
      <h3> Using These Data </h3>
      <p>
         Samples were analyzed by Dr. Nancy Johnston at <a href = 'https://www.lcsc.edu/'>Lewis Clark State college</a>.<br><br>
         At this stage, data has been quality assured to level 1.<br>
         <em>Therefore, data may change and are not finalized for archiving.</em><br><br>
         <strong>To re-use or publish these data, please <u>contact us to request permission:</u><br><br>
         THRIVEair@gmail.com</p></strong>
           </span>"))
    })
    
## Select Data Based on Date Range  ----
    data_range <- reactive({
        selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
        filter(data, end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
    })
    
    data_range_comp <- reactive({
      selected_range <- as.Date(input$date_range_comp, format = "%Y-%m-%d")
      filter(data, end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
    })
    
## Update date selections ----    
    observe({
        date_choices <- c("Average Across Date Range", format(unique(data_range()$end_date), "%m/%d/%y"))
        updateSelectInput(session, "date_filter", choices = date_choices)
    })

## Filter data ----
    filtered_data <- reactive({
        if (input$date_filter == "Average Across Date Range") {
            # Calculate monthly averages for each pollutant
            avg <- data_range() %>%
                group_by(site) %>%
                mutate(across(benzene:dodecane, mean, na.rm = TRUE)) %>%
                ungroup() %>%
                select(-start_date, -end_date) %>%
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
       
        
## HilcoDownload button ----
        output$hilcodownload <- downloadHandler(
          filename = function() {
            min_date <- min(hilco$start_date)
            max_date <- max(hilco$end_date)
            # Set the filename for the downloaded CSV
            paste("BellwetherDistrict_benzene_", min_date, "_to_", max_date, ".csv", sep = "")
          },
          content = function(file) {
            # Write the filtered data to a CSV file
            write.csv(hilco, file)
          }
        )        
        
         
    })
    


    

    
## Render Map ----
    output$map <- renderPlotly({
        # Clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
        
        # Create a custom title for the map
        title_html <- if (input$date_filter == "Average Across Date Range") {
          paste0("Average ", voc_name,": ",selected_range[1]," to ",selected_range[2])
        } else {
          paste0(voc_name,": ",input$date_filter)
        }
          
          paste0(voc_name, ": ", input$date_filter)
        
        
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
                       size = 22,
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
                    colorscale = pal,
                    reversescale = FALSE
                ),
                text = paste0(filtered_data()$site,"<br><b>",voc_name,":</b> ",round(filtered_data()[[input$pollutant]],2),HTML(" &mu;"), "g/m³"),
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
    

## Render Hilco Map ----
    output$hilcomap <- renderPlotly({
      mindate <- min(hilco$end_date)
      maxdate <- max(data$end_date)
      data2 <- data %>% 
        filter(end_date >= mindate & end_date <= maxdate) %>%
        select(1:5, lat, long) 
      combined_data <- data2 %>%
        bind_rows(., hilco) %>%
        arrange(., end_date)
      
      # Create a custom title for the map
      title_html <- paste0("Average Benzene, ", mindate," to ", maxdate)
      
      plot_mapbox(combined_data) %>%
        add_trace(
          type = "scattermapbox",
          mode = "markers",
          lat = ~lat,
          lon = ~long,
          marker = list(
            size = 22,
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
            color = combined_data$benzene,
            opacity = 1.0,
            colorbar = list(
              title = paste0("Benzene<br>"," (", HTML("&mu;"), "g/m³",")")
            ),
            colorscale = pal,
            reversescale = FALSE
          ),
          text = paste0(combined_data$site,"<br><b>Benzene:</b> ",round(combined_data$benzene,2),HTML(" &mu;"), "g/m³"),
          hoverinfo = "text",
          showlegend = FALSE
          
        ) %>%
        layout(
          mapbox = list(
            layers = list(
              below = ""
            ),
            style = "streets",
            center = list(lat = mean(combined_data$lat), lon = mean(combined_data$long)),
            zoom = 12),
          title = title_html) %>%
        config(scrollZoom = FALSE)
      
    })
    
## Render Bar Chart ----
    
    output$bar_chart <- renderPlotly({
        #clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
        
        
        #standardize color scale max across time filters
        cmax <- max(data[[input$pollutant]])
        cmin <- 0.9*min(data[[input$pollutant]])
        
        if (input$date_filter == "Average Across Date Range") {
            avg_data <- data_range() %>%
                group_by(site) %>%
                mutate(across(benzene:dodecane, mean, na.rm=TRUE)) %>%
                ungroup() %>%
                select(-start_date, -end_date) %>%
                distinct()

            plot_ly(data = avg_data, x = ~site, y = avg_data[[input$pollutant]], 
                    type = "bar", 
                    marker = list(
                        color = ~avg_data[[input$pollutant]], 
                        colorscale = pal,
                        reversescale = FALSE,
                        cmin = cmin,
                        cmax = cmax), 
                    showlegend=FALSE, 
                    text = ~avg_data$site,
                   hovertemplate = paste0(avg_data$site,"<br><b>",voc_name,":</b> ",round(avg_data[[input$pollutant]],2),HTML(" &mu;"), "g/m³","<extra></extra>")
                    ) %>%
                layout(title = paste0("Average ",voc_name, ": ", "<br>",selected_range[1]," to ", selected_range[2]),
                       xaxis = list(
                         tickfont = list(size = 15),
                         titlefont = list(size = 18),
                           title = "Site",
                           showticklabels = FALSE,
                           categoryorder = "total ascending"),
                       yaxis = list(
                         tickfont = list(size = 15),
                         titlefont = list(size = 18),
                           title = paste0(voc_name, " (", HTML("&mu;"), "g/m³",")"))
                ) %>%
              config(scrollZoom = FALSE, displaylogo = FALSE) 
        } else {
            chart_data <- filtered_data()
            
            plot_ly(data = chart_data, x = ~site, y = chart_data[[input$pollutant]], 
                    type = "bar", 
                    marker = list(
                        color = ~chart_data[[input$pollutant]], 
                        colorscale = pal,
                        reversescale = FALSE,
                        cmin = cmin,
                        cmax = cmax),
                    showlegend=FALSE, 
                    text = ~site,
                    textposition = 'auto',
                    hovertemplate = paste0(chart_data$site,"<br><b>",voc_name,":</b> ",round(chart_data[[input$pollutant]],2),HTML(" &mu;"), "g/m³","<extra></extra>")
                    ) %>%
                layout(title = paste(voc_name, ": ", "<br>", input$date_filter),
                       xaxis = list(
                         tickfont = list(size = 15),
                         titlefont = list(size = 15),
                           title = "Site",
                           showticklabels = FALSE,
                           categoryorder = "total ascending"),
                       yaxis = list(
                         tickfont = list(size = 15),
                         titlefont = list(size = 20),
                           title = paste0(voc_name, " (", HTML("&mu;"), "g/m³",")"))
                       ) %>%
              config(scrollZoom = FALSE, displaylogo = FALSE) 
        }
    }) 
    
## Render Hilco Bar Chart ----
    output$hilcobarchart <- renderPlotly({
      mindate <- min(hilco$end_date)
      maxdate <- max(data$end_date)
      data2 <- data %>% 
        filter(end_date >= mindate & end_date <= maxdate) %>%
        select(1:5, lat, long) 
      combined_data <- data2 %>%
        bind_rows(., hilco) %>%
        arrange(., end_date) %>%
        group_by(site) %>%
        mutate(benzene = mean(benzene)) %>%
        ungroup() %>%
        select(site, benzene) %>% distinct()
      
      plot_ly(data = combined_data, x = ~site, y = combined_data$benzene, 
              type = "bar", 
              marker = list(
                color = combined_data$benzene, 
                colorscale = pal,
                reversescale = FALSE), 
              showlegend=FALSE, 
              text = ~combined_data$site,
              hovertemplate = paste0(combined_data$site,"<br><b>Benzene:</b> ",round(combined_data$benzene,2),HTML(" &mu;"), "g/m³","<extra></extra>")
      ) %>%
        layout(title = paste0("Average Benzene:<br>",min(hilco$start_date)," to ", max(data$end_date)),
               xaxis = list(
                 tickfont = list(size = 15),
                 titlefont = list(size = 18),
                 title = "Site",
                 showticklabels = FALSE,
                 categoryorder = "total ascending"),
               yaxis = list(
                 tickfont = list(size = 15),
                 titlefont = list(size = 18),
                 title = paste0("Benzene (", HTML("&mu;"), "g/m³",")"))
        ) %>%
        config(scrollZoom = FALSE, displaylogo = FALSE) 
    })
    
## Render Line Chart ----

    output$line_chart <- renderPlotly({
        # Clean up variable names
        voc_name <- names(voc[voc == input$pollutant])
        # Create a title for the line chart
        line_title <- paste(voc_name)
        # Get unique site list
        sites <- unique(data_range()$site)
        
    plot <- data_range() %>% 
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
                text = paste0(data_range()$site,", ",data_range()$end_date,"<br><b>",voc_name,":</b> ",data_range()[[input$pollutant]],HTML(" &mu;"), "g/m³"), 
                hoverinfo = 'text') %>% 
            layout(
            title = line_title,
            xaxis = list(tickfont = list(size = 15),
                         titlefont = list(size = 18),
                         title = "Date"),
            yaxis = list(tickfont = list(size = 15),
                         titlefont = list(size = 18),
                         title = paste0(voc_name, " (", HTML("&mu;"), "g/m³", ")")),
            showlegend = TRUE
        ) %>%
          config(scrollZoom = FALSE, displaylogo = FALSE)
        
        # Add horizontal dashed blue line if pollutant is Benzene
        if (input$pollutant == "benzene") {
          plot <- plot %>%
            add_segments(
              x = ~min(data_range()$end_date),
              xend = ~max(data_range()$end_date),
              y = 5,
              yend = 5,
              line = list(color = "blue", width = 1, dash = "dot"),
              hoverinfo = "none",
              name = "European Regulatory Limit \n (Annual Average)"
            ) 
        }
    return(plot)

    })
    

    
    ##Render Hilco Line Charts ----
    
    output$hilcotimeseries <- renderPlotly({
      mindate <- min(hilco$end_date)
      maxdate <- max(data$end_date)
      data2 <- data %>% 
        filter(end_date >= mindate & end_date <= maxdate) %>%
        select(1:5, lat, long)  
      hilco2 <- hilco %>%
        filter(end_date <= maxdate)

     
    #  sites <- unique(data2$site)
    #  colors <- c("#1f77b4","#ff7f0f","#2ba02b","#d62727","#9467bd","#8c564c","#e377c3","#7f7f7f","#bcbd21", "#000000")
      
      data2 %>%
      plot_ly() %>%
        add_trace(
          split = ~site,
          x = ~end_date,
          y = data2$benzene,
          type = "scatter",
          mode = "markers + lines",
          name = ~site,
          connectgaps = TRUE,
          text = paste0(data2$site,"<br>",
                        data2$end_date,"<br>",
                        "Benzene</b>: ", data2$benzene, HTML(" &mu;"), "g/m³"),
          hoverinfo = 'text') %>%
         add_trace(
           x = ~hilco2$end_date,
           y = ~hilco2$benzene,
           type = "scatter",
           mode = "markers + lines",
           name = ~hilco2$site,
           connectgaps = TRUE,
           marker = list(color = '#000000'),
           line = list(color = '#000000')
         ) %>%
        # add_segments(
        #   x = ~min(data2$end_date),
        #   xend = ~max(data2$end_date),
        #   y = 5,
        #   yend = 5,
        #   line = list(color = "blue", width = 1, dash = "dot"),
        #   hoverinfo = "none",
        #   name = "European Regulatory Limit \n (Annual Average)"
        # ) %>% 
        layout(
          xaxis = list(tickfont = list(size = 15),
                       titlefont = list(size = 18),
                       title = "Date"),
          yaxis = list(tickfont = list(size = 15),
                       titlefont = list(size = 18),
                       title = paste0("Benzene (", HTML("&mu;"), "g/m³", ")")),
          showlegend = TRUE
        ) %>%
        config(scrollZoom = FALSE, displaylogo = FALSE)
      
      
   
      
    })
    
    ## Render Comparison Line Charts ----
    
    output$compchart <- renderPlotly({
      sites <- unique(data_range_comp()$site)
      colors <- c("#1f77b4","#ff7f0f","#2ba02b","#d62727","#9467bd","#8c564c","#e377c3","#7f7f7f","#bcbd21")
      selected_range <- as.Date(input$date_range_comp, format = "%Y-%m-%d")
      
      


    
fig1 <- data_range_comp() %>% 
    ungroup() %>%
    plot_ly(legendgroup = ~site, showlegend=F) %>%
    add_trace(
      color = ~site,
      colors = colors,
      x = ~end_date,
      y = ~get(input$comp1),
      type = "scatter",
      mode = "markers + lines",
      name = ~site,
      connectgaps = TRUE,
      text = paste0(data_range_comp()$site,"<br>",
                    data_range_comp()$end_date,"<br>",
                    names(voc[voc == input$comp1]),"</b>: ", data_range_comp()[[input$comp1]], HTML(" &mu;"), "g/m³"),
      hoverinfo = 'text') 
  

fig2 <- data_range_comp() %>% 
  ungroup() %>%
  plot_ly(legendgroup = ~site, showlegend=F) %>%
  add_trace(
    color = ~site,
    colors = colors,
    x = ~end_date,
    y = ~get(input$comp2),
    type = "scatter",
    mode = "markers + lines",
    name = ~site,
    connectgaps = TRUE,
    text = paste0(data_range_comp()$site,"<br>",
                  data_range_comp()$end_date,"<br>",
                  names(voc[voc == input$comp2]),"</b>: ", data_range_comp()[[input$comp2]], HTML(" &mu;"), "g/m³"), 
    hoverinfo = 'text')

fig3 <- data_range_comp() %>% 
  ungroup() %>%
  plot_ly(legendgroup = ~site, showlegend=F) %>%
  add_trace(
    color = ~site,
    colors = colors,
    x = ~end_date,
    y = ~get(input$comp3),
    type = "scatter",
    mode = "markers + lines",
    name = ~site,
    connectgaps = TRUE,
    text = paste0(data_range_comp()$site,"<br>",
                  data_range_comp()$end_date,"<br>",
                  names(voc[voc == input$comp3]),"</b>: ", data_range_comp()[[input$comp3]], HTML(" &mu;"), "g/m³"),
    hoverinfo = 'text')

fig4 <- data_range_comp() %>% 
  ungroup() %>%
  plot_ly(legendgroup = ~site, showlegend=T) %>%
  add_trace(
    color = ~site,
    colors = colors,
    x = ~end_date,
    y = ~get(input$comp4),
    type = "scatter",
    mode = "markers + lines",
    name = ~site,
    connectgaps = TRUE,
    text = paste0(data_range_comp()$site,"<br>",
                  data_range_comp()$end_date,"<br>",
                  names(voc[voc == input$comp4]),"</b>: ", data_range_comp()[[input$comp4]], HTML(" &mu;"), "g/m³"), 
    hoverinfo = 'text') 

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 4, shareX = TRUE, shareY = FALSE, titleY=TRUE) %>% 
        layout(
          xaxis = list(title = "Date"),
         # annotations = annotations,
          height = 800,
          yaxis = list(title = paste0(names(voc[voc == input$comp1]), " (", HTML("&mu;"), "g/m³", ")")),
          yaxis2 = list(title = paste0(names(voc[voc == input$comp2]), " (", HTML("&mu;"), "g/m³", ")")),
          yaxis3 = list(title = paste0(names(voc[voc == input$comp3]), " (", HTML("&mu;"), "g/m³", ")")),
          yaxis4 = list(title = paste0(names(voc[voc == input$comp4]), " (", HTML("&mu;"), "g/m³", ")"))
        ) %>%
        config(scrollZoom = FALSE, displaylogo = FALSE)
    })
    

# Render the Data Table ####  
    tab_data1 <- reactive({
      data <- read_csv("dat.csv") %>%
        filter(site_type == "stationary") %>%
        mutate(
          start_date = as.Date(start_date, format = "%m/%d/%y"),
          end_date = as.Date(end_date, format = "%m/%d/%y")
        ) %>%
        ungroup() %>%
        select(-...1) %>%
        mutate(btex = rowSums(select(data, benzene:xylenes), na.rm = TRUE)) %>%
        mutate(btex = round(btex, 3)) %>%
        relocate(btex, .after = "xylenes")
      
      if (input$flag=="Yes") {
        dat <- data
      } else if (input$flag=="No") {
        dat <- data %>% select(-ends_with("flag"))
      }
      return(dat)
    })
    
    table_data <- reactive({
        selected_range <- as.Date(input$date_range2, format = "%Y-%m-%d")
        filtered <- filter(tab_data1(), end_date %in% seq(selected_range[1], selected_range[2], by = "day")) %>%
            select(all_of(c(input$col_vars, input$col_voc)), ends_with("flag")) %>%
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
                                                     !!sym(pollutant))) %>%
                      mutate(btex = rowSums(select(filtered, benzene:xylenes), na.rm = TRUE)) %>%
                      mutate(btex = round(btex, 3)) %>%
                      relocate(btex, .after = "xylenes")
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
                                 buttons = list(
                                   list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download',
                                     exportOptions = list(
                                       modifier = list(page = "all")
                                     )
                                 )
                                 )
                                ))
                                
    }, server = FALSE)
    
    
    output$codebook <- downloadHandler(
      filename = function(){
        "THRIVEair_codebook.xlsx"},
      content = function(file) {
        file.copy("www/THRIVEair_codebook.xlsx", file)
      }
    )
    
}


    


# Run the application 
shinyApp(ui = ui, server = server)






