
#Setup -----
#Load libraries
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


##Mapbox info ----
mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
mb_access_token(mapbox_token, install = TRUE, overwrite = TRUE)

##Shiny Theme ----
my_theme = create_theme(
  adminlte_color(light_blue = "#FFF"),
  adminlte_sidebar(
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#3ace88",
    dark_color = "#2E3440"
  ),
  adminlte_global(info_box_bg = "#D8DEE9")
)




# Data Cleaning ----

# Group by site and calculate site averages (within-site, across month)
#and date average (within-date, across sites) for each pollutant


#Read in data
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

#Read in codebook
codebook <- read_excel("data/codebook.xlsx")
pollutant_names <- codebook %>%
  select(voc_name, variable_name) %>%
  rename(pollutant = variable_name)
voc <- setNames(codebook$variable_name, codebook$voc_name)
mws <- setNames(codebook$mw, codebook$variable_name)

#Create other codes
other_vars <- c(
  "Site" = "site",
  "Sample begin date" = "start_date",
  "Sample end date" = "end_date",
  "Latitude" = "lat",
  "Longitude" = "long"
)

units <- c("µg/m³", "ppbv")


#Convert data to microgram per meter cubed
for (pollutant in names(mws)) {
  if (pollutant != "btex") {
    data <- data %>%
      mutate(!!pollutant := ifelse(
        !is.na(!!sym(pollutant)),
        round(!!sym(pollutant) * mws[pollutant] / 24.45, 3),!!sym(pollutant)
      ))
  }
}

data <- data %>%
  mutate(btex = rowSums(select(data, benzene:xylenes), na.rm = TRUE)) %>%
  mutate(btex = round(btex, 3)) %>%
  relocate(btex, .after = "xylenes")


end_dates <- unique(data$end_date)

data_long <- data %>%
  pivot_longer(cols = benzene:dodecane, names_to = "pollutant") %>%
  left_join(., pollutant_names, by = "pollutant")


# User Interface ----

ui <- dashboardPage(
  ## Header ----
  dashboardHeader(title = "THRIVEair Results", tags$li(
    a(href = "https://thriveairphilly.com/", img(src = "logo.png", height = "30px")), class = "dropdown"
  )),
  ## Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        icon = icon("dashboard"),
        menuSubItem(
          HTML("THRIVEair Data:<br>All Pollutants"),
          tabName = "dashboard",
          icon = icon("dashboard"),
          selected = TRUE
        ),
        menuSubItem(
          HTML("THRIVEair +<br>Bellwether District:<br>Benzene Data"),
          tabName = "hilco",
          icon = icon("location-dot")
        )
      ),
      menuItem(
        "Compare Pollutants",
        tabName = "compare",
        icon = icon("code-compare")
      ),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem(
        "Information",
        tabName = "docs",
        icon = icon("circle-info")
      )
    ),
    tags$script(
      HTML(
        '
    $(document).ready(function() {
      // Set a static tooltip text
      $(".sidebar-toggle").attr("data-tooltip", "Toggle Navigation Pane");
    });
  '
      )
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    includeCSS("style.css"),
    
    ## Dashboard -----
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Instructions",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            includeHTML("text/dashboard_instructions.html")
          )
        ),
        
        fluidRow(
          box(
            selectInput(
              "pollutant",
              "Select Pollutant:",
              choices = voc,
              selected = "benzene"
            ),
            
            sliderTextInput(
              inputId = "date_range",
              label = "Select Monitoring End Dates:",
              choices = end_dates,
              selected = range(end_dates),
              grid = TRUE
            ),
            
            selectInput(
              "date_filter",
              "Select Date:",
              choices = NULL,
              selected = "Average Across Date Range"
            ),
            width = 6,
            title = "Select Data",
            collapsible = TRUE,
            solidHeader = TRUE,
          ),
          box(
            uiOutput("info"),
            downloadButton("download", "Download Filtered Data as .csv"),
            width = 6,
            title = "Data Summary",
            collapsible = TRUE,
            solidHeader = TRUE,
          )
        ),
        fluidRow(
          box(
            plotlyOutput("map"),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Comparing Concentrations Across Sites"
          )
        ),
        fluidRow(
          box(
            plotlyOutput("bar_chart"),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Comparing Concentrations Across Sites"
          )
        ),
        fluidRow(
          box(
            plotlyOutput("line_chart"),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Comparing Concentrations Over Time"
          )
        )
      ),
      ## Data Table -----
      tabItem(tabName = "data_table", fluidRow(
        column(
          6,
          checkboxGroupInput("col_voc", "Select Pollutant(s):", voc, selected = voc)
        ),
        column(
          6,
          checkboxGroupInput("col_vars", "Select Other Variables:", other_vars, selected = other_vars),
          sliderTextInput(
            inputId = "date_range2",
            label = "Select Monitoring End Dates:",
            choices = end_dates,
            selected = range(end_dates),
            grid = TRUE
          ),
          radioButtons("units", "Select Units", units, selected = units[1]),
          radioButtons("flag", "Show Measurement Flags?", c("Yes", "No"), selected = "Yes"),
          downloadButton("codebook", "Download Codebook"),
          HTML("<br><br>"),
          box(includeHTML("text/table_text.html"), width = 100)
          
        )
      ), fluidRow(
        box(dataTableOutput("table"), style = "overflow-x: scroll", width = 100)
      )),
      ## Information ----
      tabItem(tabName = "docs", fluidRow(
        img(
          src = 'combo_logo.png',
          height = "50%",
          width = "50%",
          align = 'center'
        ),
        width = 12
      ), fluidRow(box(
        includeHTML("text/about_text.html"), width = 12
      ))),
      ## Comparison ----
      tabItem(tabName = "compare", fluidRow(
        box(
          selectInput(
            "comp1",
            "Select Pollutant 1:",
            choices = voc,
            selected = "benzene"
          ),
          selectInput(
            "comp2",
            "Select Pollutant 2:",
            choices = voc,
            selected = "toluene"
          ),
          selectInput(
            "comp3",
            "Select Pollutant 3:",
            choices = voc,
            selected = "etbenz"
          ),
          selectInput(
            "comp4",
            "Select Pollutant 4:",
            choices = voc,
            selected = "xylenes"
          ),
          width = 6,
          title = "Select Pollutants for Comparison",
          collapsible = TRUE,
          solidHeader = TRUE,
        ),
        box(
          sliderTextInput(
            inputId = "date_range_comp",
            label = "Select Monitoring End Dates:",
            choices = end_dates,
            selected = range(end_dates),
            grid = TRUE
          ),
          width = 6,
          title = "Select Dates",
          collapsible = TRUE,
          solidHeader = TRUE,
        )
      ), fluidRow(
        box(
          plotlyOutput("compchart"),
          width = 100,
          height = 820
        ),
      )),
      ## Hilco ----
      tabItem(
        tabName = "hilco",
        fluidRow(
          box(
            title = "What is this?",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            includeHTML("text/hilco_instructions.html"),
            downloadButton("hilcodownload", "Download Bellwether District Data"),
            HTML(
              "<em><br>To download THRIVEair data, visit the Data Table page.</em>"
            )
          )
        ),
        box(
          title = "Map",
          plotlyOutput("hilcomap"),
          width = 100,
          collapsible = TRUE
        ),
        box(
          title = "Bar Chart",
          plotlyOutput("hilcobarchart"),
          width = 100,
          collapsible = TRUE
        ),
        box(
          title = "Changes Over Time",
          plotlyOutput("hilcotimeseries"),
          width = 100,
          collabsible = TRUE
        )
      )
      
    )
  )
)




#  Server ----

server <- function(input, output, session) {
  
  ## Globals ----
  pal <- list(c(0, "lightpink"), c(1, "darkblue"))
  
  ## Dashboard -----
  
  ### Update date selections ----    
  observe({
    data_range <- data_long %>%
      filter(pollutant == input$pollutant) 
    
    date_choices <- c("Average Across Date Range", format(unique(data_range$end_date), "%m/%d/%y"))
    updateSelectInput(session, "date_filter", choices = date_choices)
  })
  
  ### Reactive Values ----
  
  #Filter data based on selections
  dat <- reactive({
    selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
    
    dat1 <- data_long %>%
      filter(pollutant == input$pollutant) %>%
      filter(end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
    
    if (input$date_filter == "Average Across Date Range") {
      avg <- dat1 %>%
        group_by(site_id) %>%
        mutate(value = mean(value)) %>%
        ungroup() %>%
        distinct(site, .keep_all = TRUE)
      return(avg)
    } else {
      selected_date <- as.Date(input$date_filter, "%m/%d/%y")
      return(filter(dat1, end_date == selected_date))
    }
    
  })
  
  #Formalize VOC name based on selection
  voc_name <- reactive({
    names(voc[voc == input$pollutant])
  })
  
  
  selected_range <- reactive({
    as.Date(input$date_range, format = "%Y-%m-%d")
  })
  
  #Standardize color scale max across time filters
  cmax <- reactive({
    max(data[[input$pollutant]])
  })
  cmin <- reactive({
    0.9*min(data[[input$pollutant]])
  })
  
  
  #Reactive summary text
  observe({
    
    # Calculate the average concentration for the selected range
    avg_selected_range <- round(mean(dat()$value), 2)
    
    # Calculate the study-wide average concentration for the selected pollutant
    avg_study_wide <- data_long %>% 
      filter(pollutant == input$pollutant) %>%
      pull(value) %>%
      mean(., na.rm = TRUE) %>%
      round(., 2)
    
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
        HTML(paste("The average concentration of", voc_name(), 
                   "for the selected date range was <strong>", round(avg_selected_range, 2), ("&mu;"), "g/m³",
                   "</strong>.", "This is", comparison, "the study-wide average of", 
                   "<strong>", round(avg_study_wide, 2), ("&mu;"), "g/m³","</strong>."))
      } else {
        HTML(paste("The average concentration of", voc_name(), 
                   "for the week ending on", input$date_filter, "was <strong>", round(avg_selected_range, 2), ("&mu;"), "g/m³",
                   "</strong>.", "This is", comparison, "the study-wide average of", 
                   "<strong>", round(avg_study_wide, 2), ("&mu;"), "g/m³","</strong>."))
      }
    })
  })
  
  
  ### Map ----
  output$map <- renderPlotly({
    
    #  selected_range() <- as.Date(input$date_range, format = "%Y-%m-%d")
    
    # Create a custom title for the map
    
    title_text <- if(input$date_filter == "Average Across Date Range"){
      paste0("Average ",voc_name(), ": ", "<br>",selected_range()[1]," to ", selected_range()[2])
    } else{
      paste(voc_name(), ": ", "<br>", first(dat()$start_date)," to ", first(dat()$end_date))
    }
    
    
    #
    
    # Render the map  
    plot_mapbox(dat()) %>%
      add_trace(
        type = "scattermapbox",
        mode = "markers",
        lat = ~ lat,
        lon = ~ long,
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
        lat = ~ lat,
        lon = ~ long,
        marker = list(
          size = 20,
          color = dat()$value,
          opacity = 1.0,
          cmin = cmin(),
          cmax = cmax(),
          colorbar = list(title = paste0(
            voc_name(), "<br>", " (", HTML("&mu;"), "g/m³", ")"
          )),
          colorscale = pal,
          reversescale = FALSE
        ),
        text = paste0(
          # dat$site,
          "<br><b>",
          voc_name(),
          ":</b> ",
          #  round(dat()[value], 2),
          HTML(" &mu;"),
          "g/m³"
        ),
        hoverinfo = "text",
        showlegend = FALSE
        
      ) %>%
      layout(
        mapbox = list(
          layers = list(below = ""),
          style = "streets",
          center = list(lat = mean(data$lat), lon = mean(data$long)),
          zoom = 12
        ),
        title = title_text
      ) %>%
      config(scrollZoom = FALSE)
  })
  
  
  ### Bar Chart ----
  output$bar_chart <- renderPlotly({
    
    hover_text <- if(input$date_filter == "Average Across Date Range"){
      paste0(dat()$site,"<br><b>",voc_name(),":</b> ",round(dat()$value,2),HTML(" &mu;"), "g/m³","<extra></extra>")
    } else {
      paste0(dat()$site,"<br><b>",voc_name(),":</b> ",round(dat()$value,2),HTML(" &mu;"), "g/m³","<extra></extra>")
    }
    
    title_text <- if(input$date_filter == "Average Across Date Range"){
      paste0("Average ",voc_name(), ": ", "<br>",selected_range()[1]," to ", selected_range()[2])
    } else{
      paste(voc_name(), ": ", "<br>", first(dat()$start_date)," to ", first(dat()$end_date))
    }
    
    plot_ly(data = dat(), x = ~site, y = dat()$value, 
            type = "bar", 
            marker = list(
              color = dat()$value, 
              colorscale = pal,
              reversescale = FALSE,
              cmin = cmin(),
              cmax = cmax()), 
            showlegend=FALSE, 
            text = dat()$site,
            hovertemplate = hover_text
    ) %>%
      layout(title = title_text,
             xaxis = list(
               tickfont = list(size = 15),
               titlefont = list(size = 18),
               title = "Site",
               showticklabels = FALSE,
               categoryorder = "total ascending"),
             yaxis = list(
               tickfont = list(size = 15),
               titlefont = list(size = 18),
               title = paste0(voc_name(), " (", HTML("&mu;"), "g/m³",")"))
      ) %>%
      config(scrollZoom = FALSE, displaylogo = FALSE) 
    
  }) 
  
  
  ### Line Chart ----
  
  output$line_chart <- renderPlotly({
    
    data_range <- data_long %>%
      filter(pollutant == input$pollutant) %>%
      filter(end_date %in% seq(selected_range()[1], selected_range()[2], by = "day")) %>%
      ungroup()
    
    
    # Get unique site list
    sites <- unique(data_range$site)
    
    plot <- plot_ly(data_range) %>%
      add_trace(
        split = ~site,
        x = ~end_date,
        y = ~value,
        type = "scatter",
        mode = "markers + lines",
        name = ~site,
        connectgaps = TRUE,
        text = paste0(data_range$site,", ",data_range$end_date,"<br><b>",voc_name(),":</b> ",data_range$value,HTML(" &mu;"), "g/m³"), 
        hoverinfo = 'text') %>% 
      layout(
        title = voc_name(),
        xaxis = list(tickfont = list(size = 15),
                     titlefont = list(size = 18),
                     title = "Date"),
        yaxis = list(tickfont = list(size = 15),
                     titlefont = list(size = 18),
                     title = paste0(voc_name(), " (", HTML("&mu;"), "g/m³", ")")),
        showlegend = TRUE
      ) %>%
      config(scrollZoom = FALSE, displaylogo = FALSE)
    
    # Add horizontal dashed blue line if pollutant is Benzene
    if (input$pollutant == "benzene") {
      plot <- plot %>%
        add_segments(
          x = min(data_range$end_date),
          xend = max(data_range$end_date),
          y = 5,
          yend = 5,
          line = list(color = "blue", width = 1, dash = "dot"),
          hoverinfo = "none",
          name = "European Regulatory Limit \n (Annual Average)"
        ) 
    }
    return(plot)
    
  })
  
  
  ### Download button ----
  output$download <- downloadHandler(
    filename = function() {
      selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
      min_date <- selected_range[1]
      max_date <- selected_range[2]
      # Set the filename for the downloaded CSV
      paste("THRIVEair_results_", min_date, "_to_", max_date, ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- dat() %>%
        select(-voc_name) %>%
        pivot_wider(names_from = pollutant, values_from = value)
      # Write the filtered data to a CSV file
      write.csv(filtered_data, file)
    }
  )
  
  
  ## Hilco -----
  
  ### Map ----
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
  
  
  
  ### Bar Chart ----
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
  
  
  
  
  ### Line Chart ----
  
  output$hilcotimeseries <- renderPlotly({
    mindate <- min(hilco$end_date)
    maxdate <- max(data$end_date)
    data2 <- data %>% 
      filter(end_date >= mindate & end_date <= maxdate) %>%
      select(1:5, lat, long)  
    hilco2 <- hilco %>%
      filter(end_date <= maxdate)
    
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
  
  ### Download button ----
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
  
  
  ## Comparison -----
  data_range_comp <- reactive({
    selected_range <- as.Date(input$date_range_comp, format = "%Y-%m-%d")
    filter(data, end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
  })
  
  ## Render Comparison Line Charts ----
  
  output$compchart <- renderPlotly({
    sites <- unique(data_range_comp()$site)
    colors <- c("#1f77b4","#ff7f0f","#2ba02b","#d62727","#9467bd","#8c564c","#e377c3","#7f7f7f","#bcbd21")
    
    fig_func <- function(selected_pollutant){
      selected_range <- as.Date(input$date_range_comp, format = "%Y-%m-%d")
      
      dat <- data_long %>%
        filter(end_date %in% seq(selected_range[1], selected_range[2], by = "day")) %>%
        ungroup() %>%
        filter(pollutant == selected_pollutant) 
      
      fig <- dat %>%
        plot_ly(legendgroup = ~site, showlegend=F) %>%
        add_trace(
          color = ~site,
          colors = colors,
          x = ~end_date,
          y = ~value,
          type = "scatter",
          mode = "markers + lines",
          name = ~site,
          connectgaps = TRUE,
          text = paste0(dat$site,"<br>",
                        dat$end_date,"<br>",
                        dat$voc_name,"</b>: ", dat$value, HTML(" &mu;"), "g/m³"),
          hoverinfo = 'text')
    }
    
    fig <- subplot(fig_func(input$comp1), 
                   fig_func(input$comp2), 
                   fig_func(input$comp3), 
                   fig_func(input$comp4), 
                   nrows = 4, shareX = TRUE, shareY = FALSE, titleY=TRUE) %>% 
      layout(
        xaxis = list(title = "Date"),
        height = 800,
        yaxis = list(title = paste0(names(voc[voc == input$comp1]), " (", HTML("&mu;"), "g/m³", ")")),
        yaxis2 = list(title = paste0(names(voc[voc == input$comp2]), " (", HTML("&mu;"), "g/m³", ")")),
        yaxis3 = list(title = paste0(names(voc[voc == input$comp3]), " (", HTML("&mu;"), "g/m³", ")")),
        yaxis4 = list(title = paste0(names(voc[voc == input$comp4]), " (", HTML("&mu;"), "g/m³", ")"))
      ) %>%
      config(scrollZoom = FALSE, displaylogo = FALSE)
  })
  
  
  
  ## Data Table -----
  ### Render the Data Table ####  
  tab_data1 <- reactive({
    data <- read_csv("data/clean/dat.csv") %>%
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


# Run app ----
shinyApp(ui = ui, server = server)






