#Setup -----
#Load libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(ggiraph)
library(leaflet)
library(shinydashboard)
library(lubridate)
library(DT)
library(shinyWidgets)
library(fresh)
library(readxl)
library(scales)

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

## Palette used everywhere (map fills, bar fills, legends) ----
pal_colors <- c("lightpink", "darkblue")
highlight_color <- "#FFD700"


# Data Cleaning ----

# Group by site and calculate site averages (within-site, across month)
#and date average (within-date, across sites) for each pollutant


#Read in data
data <- readRDS("data/dat_mgm3.rds") %>%
  filter(site_type == "stationary") %>%
  mutate(
    start_yr_temp = str_sub(start_date, 1, 4),
    start_mo_temp = str_sub(start_date, 6,7),
    start_day_temp = str_sub(start_date, -2,-1),
    end_yr_temp = str_sub(end_date, 1, 4),
    end_mo_temp = str_sub(end_date, 6,7),
    end_day_temp = str_sub(end_date, -2,-1),
    start_date = lubridate::make_date(year = start_yr_temp, month = start_mo_temp, day = start_day_temp),
    end_date = lubridate::make_date(year = end_yr_temp, month = end_mo_temp, day = end_day_temp)
  ) %>%
  ungroup() %>%
  select(-ends_with("_flag"), - ends_with ("_temp")) %>%
  arrange(., end_date)

#Hilco benzene data, already in micrograms per meter cubed
hilco <- readRDS("data/hilco.rds") %>%
  arrange(., end_date) %>%
  mutate(
    benzene = as.numeric(benzene),
    lat = as.numeric(trimws(lat)),
    long = as.numeric(trimws(long))
  )

#Read in site colors
site_cols <- readRDS("data/sitenames.rds") %>%
  select(site_id, site_color)

#Read in codebook
codebook <- readRDS("data/codebook.rds")
pollutant_names <- codebook %>%
  select(voc_name, variable_name) %>%
  rename(pollutant = variable_name)
voc <- setNames(codebook$variable_name, codebook$voc_name)
mws <- setNames(codebook$mw, codebook$variable_name)

#Read in compound info blurb fields (sources, health effects, regulatory status, links)
voc_info <- readRDS("data/voc_info.rds")

#Create other codes
other_vars <- c(
  "Site" = "site",
  "Sample begin date" = "start_date",
  "Sample end date" = "end_date",
  "Latitude" = "lat",
  "Longitude" = "long"
)

units <- c("µg/m³", "ppbv")

end_dates <- unique(data$end_date)

data_long <- data %>%
  pivot_longer(cols = m_p_xylene_2:btex, names_to = "pollutant") %>%
  left_join(., pollutant_names, by = "pollutant") %>%
  left_join(., site_cols, by = "site_id")

## Site color lookup ----
## One canonical site -> hex color mapping, built off the same site_color
## column used for the maps, so line charts, bar charts, and maps all agree.
## `distinct()` first in case a site's color varies across rows for any reason.
site_palette <- data_long %>%
  distinct(site, site_color) %>%
  { setNames(.$site_color, .$site) }

## Hilco/Bellwether District isn't in site_cols (it's a separate data source
## plotted in black), so extend the palette with it for the Hilco charts.
hilco_site_name <- unique(hilco$site)
site_palette_hilco <- c(site_palette, setNames(rep("black", length(hilco_site_name)), hilco_site_name))


# Helper functions ----

## Build a leaflet site map. `df` must have site, lat, long, and a numeric
## `fill_col` column to color by (or NULL for a fixed site_color column).
## outputId is the id used for click events / proxy linking.
leaflet_site_map <- function(df, fill_col = NULL, fixed_color_col = NULL,
                             legend_title = NULL) {
  
  base_center <- list(lat = mean(df$lat, na.rm = TRUE), lng = mean(df$long, na.rm = TRUE))
  
  m <- leaflet(df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = base_center$lng, lat = base_center$lat, zoom = 13)
  
  if (!is.null(fill_col)) {
    pal_fun <- colorNumeric(pal_colors, domain = fill_col, na.color = "grey70")
    m <- m %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        layerId = ~site,
        radius = 10,
        color = "black",
        weight = 2,
        fillColor = pal_fun(fill_col),
        fillOpacity = 1,
        opacity = 1,
        label = ~paste0(site, ": ", round(fill_col, 2), " µg/m³")
      ) %>%
      addLegend(
        "bottomright",
        pal = pal_fun,
        values = fill_col,
        title = legend_title %||% "µg/m³",
        opacity = 1
      )
  } else {
    if (!fixed_color_col %in% names(df)) {
      stop("leaflet_site_map(): column '", fixed_color_col, "' not found in df")
    }
    fill_vals <- df[[fixed_color_col]]
    fill_vals[is.na(fill_vals)] <- "grey70"
    
    m <- m %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        layerId = ~site,
        radius = 10,
        color = "black",
        weight = 2,
        fillColor = fill_vals,
        fillOpacity = 1,
        opacity = 1,
        label = ~site
      )
  }
  
  m
}

## Build an interactive ggiraph bar chart, ordered ascending, colored by value.
girafe_bar <- function(df, value_col = "value", label_col = "site",
                       y_title = "µg/m³", title_text = "") {
  
  df <- df %>%
    mutate(.tooltip = paste0(.data[[label_col]], "<br><b>", round(.data[[value_col]], 2), "</b> µg/m³"))
  
  p <- ggplot(df, aes(
    x = reorder(.data[[label_col]], .data[[value_col]]),
    y = .data[[value_col]],
    fill = .data[[value_col]],
    tooltip = .tooltip,
    data_id = .data[[label_col]]
  )) +
    geom_col_interactive(color = "grey30", linewidth = 0.2) +
    scale_fill_gradient(low = pal_colors[1], high = pal_colors[2], guide = "none") +
    labs(title = title_text, x = NULL, y = y_title) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = 15)
    )
  
  girafe(
    ggobj = p,
    width_svg = 10, height_svg = 4.5,
    options = list(
      opts_selection(type = "multiple", css = girafe_css(css = paste0("stroke:", highlight_color, ";stroke-width:3px;"))),
      opts_hover(css = "stroke:black;stroke-width:2px;"),
      opts_hover_inv(css = "opacity:0.4;"),
      opts_tooltip(opacity = 0.9, use_fill = TRUE),
      opts_toolbar(saveaspng = FALSE)
    )
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a


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
        "Information",
        tabName = "docs",
        icon = icon("circle-info"),
        selected = TRUE
      ),
      menuItem(
        "Dashboard",
        icon = icon("dashboard"),
        menuSubItem(
          HTML("THRIVEair Data:<br>All Pollutants"),
          tabName = "dashboard",
          icon = icon("dashboard")
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
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
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
      ## Information ----
      tabItem(tabName = "docs", fluidRow(box(
        includeHTML("text/about_text.html"), width = 12
      ))
      ),
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
            uiOutput("compound_info"),
            width = 100,
            title = "About This Compound",
            collapsible = TRUE,
            solidHeader = TRUE,
          )
        ),
        fluidRow(
          box(
            leafletOutput("map", height = 500),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Comparing Concentrations Across Sites"
          )
        ),
        fluidRow(
          box(
            girafeOutput("bar_chart"),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Comparing Concentrations Across Sites"
          )
        ),
        fluidRow(
          box(
            girafeOutput("line_chart"),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Comparing Concentrations Over Time"
          )
        ),
        fluidRow(
          box(
            leafletOutput("locationmap", height = 500),
            width = 100,
            collapsible = TRUE,
            solidHeader = TRUE,
            title = "Site Locations"
          )
        )
      ),
      ## Data Table -----
      tabItem(tabName = "data_table", fluidRow(
        column(
          6,
          selectInput(
            inputId = "col_voc",
            label = "Select Pollutant(s):",
            choices = voc,
            selected = voc,
            multiple = TRUE
          ),
          
          actionButton("select_all", "Select All"),
          actionButton("deselect_all", "Deselect All"),
          
          HTML("<br><br>")
          
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
          downloadButton("codebook", "Download Codebook")
        )
      ), fluidRow(
        box(includeHTML("text/table_text.html"), width = 100)
      ),
      fluidRow(
        box(dataTableOutput("table"), style = "overflow-x: scroll", width = 100)
      )),
      
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
            selected = "ethylbenzene"
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
          girafeOutput("compchart", height = "820px"),
          width = 100
        ),
      ),
      fluidRow(
        box(
          leafletOutput("locationmap2", height = 500),
          width = 100,
          collapsible = TRUE,
          solidHeader = TRUE,
          title = "Site Locations"
        )
      )
      ),
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
          leafletOutput("hilcomap", height = 500),
          width = 100,
          collapsible = TRUE
        ),
        box(
          title = "Bar Chart",
          girafeOutput("hilcobarchart"),
          width = 100,
          collapsible = TRUE
        ),
        box(
          title = "Changes Over Time",
          girafeOutput("hilcotimeseries"),
          width = 100,
          collapsible = TRUE
        )
      )
      
    )
  )
)




#  Server ----

server <- function(input, output, session) {
  
  ## Globals ----
  pal <- list(c(0, "lightpink"), c(1, "darkblue"))
  
  ## Cross-widget linking helper -----
  ## Keeps a set of ggiraph widgets and leaflet maps in sync on a shared
  ## `site` data_id / layerId. Selecting/hovering-and-clicking in any member
  ## broadcasts the selected site(s) to every other member of the group.
  sync_group <- function(girafe_ids = character(0),
                         leaflet_ids = character(0),
                         leaflet_data = NULL) {
    
    broadcast <- function(sel, exclude_girafe = NULL, exclude_leaflet = NULL) {
      for (id in setdiff(girafe_ids, exclude_girafe)) {
        session$sendCustomMessage(type = paste0(id, "_set"), message = sel)
      }
      for (id in setdiff(leaflet_ids, exclude_leaflet)) {
        proxy <- leafletProxy(id, session = session)
        proxy %>% clearGroup("highlight")
        if (length(sel) > 0 && !is.null(leaflet_data)) {
          hl <- leaflet_data() %>% filter(site %in% sel)
          if (nrow(hl) > 0) {
            proxy %>% addCircleMarkers(
              lng = hl$long, lat = hl$lat, radius = 16,
              color = highlight_color, weight = 3, fill = FALSE,
              group = "highlight"
            )
          }
        }
      }
    }
    
    lapply(girafe_ids, function(source_id) {
      observeEvent(input[[paste0(source_id, "_selected")]], {
        broadcast(input[[paste0(source_id, "_selected")]], exclude_girafe = source_id)
      }, ignoreNULL = FALSE)
    })
    
    lapply(leaflet_ids, function(source_id) {
      observeEvent(input[[paste0(source_id, "_marker_click")]], {
        clicked <- input[[paste0(source_id, "_marker_click")]]$id
        broadcast(clicked, exclude_leaflet = source_id)
      })
    })
  }
  
  ## Dashboard -----
  
  ### Reactive Values ----
  
  observeEvent(input$select_all, {
    updateSelectInput(session, "col_voc", selected = voc)
  })
  
  observeEvent(input$deselect_all, {
    updateSelectInput(session, "col_voc", selected = character(0))
  })
  
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
  
  ### Update date selections ----
  observe({
    selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
    
    data_range <- data_long %>%
      filter(end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
    
    date_choices <- c("Average Across Date Range", format(unique(data_range$end_date), "%m/%d/%y"))
    updateSelectInput(session, "date_filter", choices = date_choices)
  })
  
  #Formalize VOC name based on selection
  voc_name <- reactive({
    names(voc[voc == input$pollutant])
  })
  
  
  ### About This Compound text box ----
  output$compound_info <- renderUI({
    info <- voc_info %>% filter(variable_name == input$pollutant)
    
    if (nrow(info) == 0) {
      return(HTML("<p><em>No information available for this compound yet.</em></p>"))
    }
    
    name <- voc_name()
    
    HTML(paste0(
      "<p><strong>", name, "</strong> is ", info$what_it_is, "</p>",
      "<p>Its common sources are: ", info$common_sources, ".</p>",
      "<p>When people inhale ", name, ", it can cause these health outcomes: ",
      info$health_effects, ".</p>",
      "<p>", name, " is <strong>", info$regulated_answer, "</strong> regulated in outdoor air. ",
      info$regulated_detail, "</p>",
      "<p>The scientific evidence about how outdoor ", name,
      " in the air affects health is <strong>", tolower(info$evidence_level), "</strong>. ",
      info$evidence_detail, "</p>",
      "<p><a href='", info$more_info_link, "' target='_blank' rel='noopener noreferrer'>",
      "Learn more about ", name, "</a></p>"
    ))
  })
  
  
  selected_range <- reactive({
    as.Date(input$date_range, format = "%Y-%m-%d")
  })
  
  
  #Reactive summary text
  observe({
    
    avg_selected_range <- round(mean(dat()$value), 2)
    
    avg_study_wide <- data_long %>%
      filter(pollutant == input$pollutant) %>%
      pull(value) %>%
      mean(., na.rm = TRUE) %>%
      round(., 2)
    
    comparison <- if (is.na(avg_selected_range) || is.na(avg_study_wide)) {
      "N/A"
    } else if (avg_selected_range > avg_study_wide) {
      "<span style='color:red;font-weight:bold;'>higher than </span>"
    } else if (avg_selected_range < avg_study_wide) {
      "<span style='color:green;font-weight:bold;'>lower than </span>"
    } else {
      "<span style='color:black;'>the same as </span>"
    }
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
  output$map <- renderLeaflet({
    leaflet_site_map(dat(), fill_col = dat()$value, legend_title = voc_name())
  })
  
  ### Bar Chart ----
  output$bar_chart <- renderGirafe({
    title_text <- if (input$date_filter == "Average Across Date Range") {
      paste0("Average ", voc_name(), ": ", selected_range()[1], " to ", selected_range()[2])
    } else {
      paste0(voc_name(), ": ", first(dat()$start_date), " to ", first(dat()$end_date))
    }
    
    girafe_bar(dat(), value_col = "value", label_col = "site",
               y_title = paste0(voc_name(), " (µg/m³)"),
               title_text = title_text)
  })
  
  
  ### Line Chart ----
  output$line_chart <- renderGirafe({
    
    data_range <- data_long %>%
      filter(pollutant == input$pollutant) %>%
      filter(end_date %in% seq(selected_range()[1], selected_range()[2], by = "day")) %>%
      ungroup() %>%
      mutate(.tooltip = paste0(site, ", ", end_date, "<br><b>", voc_name(), ":</b> ", round(value, 2), " µg/m³"))
    
    p <- ggplot(data_range, aes(
      x = end_date, y = value, color = site, group = site,
      data_id = site, tooltip = .tooltip
    )) +
      geom_line_interactive(linewidth = 0.7) +
      geom_point_interactive(size = 2) +
      scale_color_manual(values = site_palette) +
      labs(title = voc_name(), x = "Date", y = paste0(voc_name(), " (µg/m³)"), color = "Site") +
      theme_minimal(base_size = 13)
    
    if (input$pollutant == "benzene") {
      p <- p + geom_hline(yintercept = 5, linetype = "dotted", color = "blue") +
        annotate("text", x = max(data_range$end_date), y = 5,
                 label = "EU Regulatory Limit (Annual Avg)", hjust = 1, vjust = -0.5, size = 3, color = "blue")
    }
    
    girafe(
      ggobj = p,
      width_svg = 10, height_svg = 5,
      options = list(
        opts_selection(type = "multiple", css = girafe_css(css = paste0("stroke:", highlight_color, ";stroke-width:4px;"))),
        opts_hover(css = "stroke-width:3px;"),
        opts_hover_inv(css = "opacity:0.25;"),
        opts_tooltip(opacity = 0.9),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })
  
  
  ### Download button ----
  output$download <- downloadHandler(
    filename = function() {
      selected_range <- as.Date(input$date_range, format = "%Y-%m-%d")
      min_date <- selected_range[1]
      max_date <- selected_range[2]
      paste("THRIVEair_results_", min_date, "_to_", max_date, ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- dat() %>%
        select(-voc_name) %>%
        pivot_wider(names_from = pollutant, values_from = value)
      write.csv(filtered_data, file)
    }
  )
  
  
  ### Site Location Map ----
  output$locationmap <- renderLeaflet({
    leaflet_site_map(dat(), fixed_color_col = "site_color")
  })
  
  ### Site Location Map 2----
  output$locationmap2 <- renderLeaflet({
    leaflet_site_map(data_range_comp(), fixed_color_col = "site_color")
  })
  
  
  ## Hilco -----
  
  hilco_map_data <- reactive({
    mindate <- min(hilco$end_date)
    maxdate <- max(data$end_date)
    data2 <- data %>%
      filter(end_date >= mindate & end_date <= maxdate) %>%
      select(site, site_id, start_date, end_date, lat, long, benzene)
    data2 %>%
      bind_rows(., hilco) %>%
      arrange(., end_date)
  })
  
  ### Map ----
  output$hilcomap <- renderLeaflet({
    combined_data <- hilco_map_data() %>%
      group_by(site) %>%
      mutate(benzene = mean(benzene, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(site, .keep_all = TRUE)
    
    leaflet_site_map(combined_data, fill_col = combined_data$benzene, legend_title = "Benzene")
  })
  
  
  ### Bar Chart ----
  output$hilcobarchart <- renderGirafe({
    combined_data <- hilco_map_data() %>%
      group_by(site) %>%
      mutate(benzene = mean(benzene, na.rm = TRUE)) %>%
      ungroup() %>%
      select(site, benzene) %>%
      distinct()
    
    girafe_bar(combined_data, value_col = "benzene", label_col = "site",
               y_title = "Benzene (µg/m³)",
               title_text = paste0("Average Benzene: ", min(hilco$start_date), " to ", max(data$end_date)))
  })
  
  
  ### Line Chart ----
  output$hilcotimeseries <- renderGirafe({
    mindate <- min(hilco$end_date)
    maxdate <- max(data$end_date)
    data2 <- data %>%
      filter(end_date >= mindate & end_date <= maxdate) %>%
      select(site, site_id, start_date, end_date, lat, long, benzene) %>%
      mutate(.tooltip = paste0(site, "<br>", end_date, "<br><b>Benzene:</b> ", round(benzene, 2), " µg/m³"))
    
    hilco2 <- hilco %>%
      filter(end_date <= maxdate) %>%
      mutate(.tooltip = paste0(site, "<br>", end_date, "<br><b>Benzene:</b> ", round(benzene, 2), " µg/m³"))
    
    p <- ggplot() +
      geom_line_interactive(
        data = data2,
        aes(x = end_date, y = benzene, color = site, group = site, data_id = site, tooltip = .tooltip),
        linewidth = 0.7
      ) +
      geom_point_interactive(
        data = data2,
        aes(x = end_date, y = benzene, color = site, data_id = site, tooltip = .tooltip),
        size = 2
      ) +
      geom_line_interactive(
        data = hilco2,
        aes(x = end_date, y = benzene, group = site, data_id = site, tooltip = .tooltip),
        color = "black", linewidth = 0.9
      ) +
      geom_point_interactive(
        data = hilco2,
        aes(x = end_date, y = benzene, data_id = site, tooltip = .tooltip),
        color = "black", size = 2
      ) +
      scale_color_manual(values = site_palette_hilco) +
      labs(x = "Date", y = "Benzene (µg/m³)", color = "Site") +
      theme_minimal(base_size = 13)
    
    girafe(
      ggobj = p,
      width_svg = 10, height_svg = 5,
      options = list(
        opts_selection(type = "multiple", css = girafe_css(css = paste0("stroke:", highlight_color, ";stroke-width:4px;"))),
        opts_hover(css = "stroke-width:3px;"),
        opts_hover_inv(css = "opacity:0.25;"),
        opts_tooltip(opacity = 0.9),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })
  
  ### Download button ----
  output$hilcodownload <- downloadHandler(
    filename = function() {
      min_date <- min(hilco$start_date)
      max_date <- max(hilco$end_date)
      paste("BellwetherDistrict_benzene_", min_date, "_to_", max_date, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hilco, file)
    }
  )
  
  
  ## Comparison -----
  data_range_comp <- reactive({
    selected_range <- as.Date(input$date_range_comp, format = "%Y-%m-%d")
    data %>%
      left_join(site_cols, by = "site_id") %>%
      filter(end_date %in% seq(selected_range[1], selected_range[2], by = "day"))
  })
  
  ## Render Comparison Line Chart ----
  output$compchart <- renderGirafe({
    selected_range <- as.Date(input$date_range_comp, format = "%Y-%m-%d")
    # Order matters (comp1..comp4), but duplicates collapse to one facet -
    # you can't show the same pollutant twice.
    selected_pollutants <- unique(c(input$comp1, input$comp2, input$comp3, input$comp4))
    
    validate(
      need(length(selected_pollutants) > 0, "Select at least one pollutant to compare."),
      need(any(selected_pollutants %in% data_long$pollutant),
           "None of the selected pollutants have data available.")
    )
    
    # code -> display name, built once from the same lookup used elsewhere
    code_to_name <- setNames(names(voc), voc)
    
    dat_comp <- data_long %>%
      filter(end_date %in% seq(selected_range[1], selected_range[2], by = "day")) %>%
      ungroup() %>%
      filter(pollutant %in% selected_pollutants) %>%
      mutate(
        # facet on the code itself - guaranteed unique, no name-matching to get wrong
        pollutant = factor(pollutant, levels = selected_pollutants),
        .tooltip = paste0(site, "<br>", end_date, "<br><b>", voc_name, ":</b> ", round(value, 2), " µg/m³")
      )
    
    validate(
      need(nrow(dat_comp) > 0, "No data available for the selected pollutant(s) in this date range.")
    )
    
    facet_labels <- code_to_name[levels(dat_comp$pollutant)]
    
    p <- ggplot(dat_comp, aes(
      x = end_date, y = value, color = site, group = site,
      data_id = site, tooltip = .tooltip
    )) +
      geom_line_interactive(linewidth = 0.6) +
      geom_point_interactive(size = 1.6) +
      facet_wrap(~pollutant, ncol = 1, scales = "free_y",
                 labeller = as_labeller(facet_labels)) +
      scale_color_manual(values = site_palette) +
      labs(x = "Date", y = "Concentration (µg/m³)", color = "Site") +
      theme_minimal(base_size = 13) +
      theme(strip.text = element_text(face = "bold", size = 13))
    
    girafe(
      ggobj = p,
      width_svg = 10, height_svg = 11,
      options = list(
        opts_selection(type = "multiple", css = girafe_css(css = paste0("stroke:", highlight_color, ";stroke-width:4px;"))),
        opts_hover(css = "stroke-width:3px;"),
        opts_hover_inv(css = "opacity:0.25;"),
        opts_tooltip(opacity = 0.9),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })
  
  
  
  ## Data Table -----
  ### Render the Data Table ####
  tab_data1 <- reactive({
    
    if (input$units == "µg/m³"){
      data <- readRDS("data/dat_mgm3.rds")
    } else {
      data <- readRDS("data/dat_ppb.rds")
    }
    
    clean_dat <- data %>%
      mutate(
        start_yr_temp = str_sub(start_date, 1, 4),
        start_mo_temp = str_sub(start_date, 6,7),
        start_day_temp = str_sub(start_date, -2,-1),
        end_yr_temp = str_sub(end_date, 1, 4),
        end_mo_temp = str_sub(end_date, 6,7),
        end_day_temp = str_sub(end_date, -2,-1),
        start_date = lubridate::make_date(year = start_yr_temp, month = start_mo_temp, day = start_day_temp),
        end_date = lubridate::make_date(year = end_yr_temp, month = end_mo_temp, day = end_day_temp)
      ) %>%
      select(-ends_with("_temp"))
    return(clean_dat)
  })
  
  
  table_data <- reactive({
    selected_range <- as.Date(input$date_range2, format = "%Y-%m-%d")
    
    voc_cols <- input$col_voc
    flag_cols <- paste0(voc_cols, "_flag")
    flag_cols <- flag_cols[flag_cols %in% colnames(tab_data1())]
    
    if (input$flag == "No") flag_cols <- character(0)
    
    interleaved_cols <- c()
    for (i in seq_along(voc_cols)) {
      interleaved_cols <- c(interleaved_cols, voc_cols[i])
      if (flag_cols[i] %in% flag_cols) {
        interleaved_cols <- c(interleaved_cols, flag_cols[i])
      }
    }
    
    select_cols <- c(input$col_vars, interleaved_cols)
    
    filtered <- tab_data1() %>%
      filter(end_date %in% seq(selected_range[1], selected_range[2], by = "day")) %>%
      select(site, site_type, lat, long, start_date, end_date, all_of(select_cols)) %>%
      mutate(across(all_of(voc_cols), round, 3))
    
    return(filtered)
  })
  
  
  output$table <- renderDataTable({
    datatable(table_data(),
              extensions = 'Buttons',
              options = list(scrollX=TRUE,
                             paging = TRUE,
                             searching = TRUE,
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
  
  
  ## Wire up cross-widget linking -----
  sync_group(
    girafe_ids  = c("bar_chart", "line_chart"),
    leaflet_ids = c("map", "locationmap"),
    leaflet_data = dat
  )
  
  sync_group(
    girafe_ids  = c("hilcobarchart", "hilcotimeseries"),
    leaflet_ids = c("hilcomap"),
    leaflet_data = hilco_map_data
  )
  
  sync_group(
    girafe_ids  = c("compchart"),
    leaflet_ids = c("locationmap2"),
    leaflet_data = data_range_comp
  )
  
}


# Run app ----
shinyApp(ui = ui, server = server)