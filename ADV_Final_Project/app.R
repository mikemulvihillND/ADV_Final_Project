suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(osrm))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(ggplot2))
options(warn = -1)

## Load necessary data
phone_calls <- readRDS("data/phone_calls.rds")
facilities_3857 <- readRDS("data/facilities_3857.rds")
census_3857 <- readRDS("data/census_3857.rds")
districts_sf <- readRDS("data/districts_sf.rds")
businesses_indiana_districts <- readRDS("data/businesses_indiana_districts.rds")
school_boundaries_3857 <- readRDS("data/school_boundaries_3857.rds")
parks_3857 <- readRDS("data/parks_3857.rds")
street_lights_3857 <- readRDS("data/street_lights_3857.rds")
all_routes_nested <- readRDS("data/school_park_routes.rds")

meters_to_sq_miles <- 3.861e-7
meters_per_mile <- 1609.34

## Pre-compute KPIs for dashboard
total_calls <- nrow(phone_calls)
total_businesses <- nrow(businesses_indiana_districts)
total_parks <- nrow(parks_3857)
total_facilities <- nrow(facilities_3857)
total_schools <- nrow(school_boundaries_3857)
total_street_lights <- nrow(street_lights_3857)
total_districts <- nrow(districts_sf)
total_population <- sum(census_3857$A00001_1, na.rm = TRUE)

## UI using shinydashboard
ui <- dashboardPage(
  skin = "blue",
  
  ## Dashboard Header
  dashboardHeader(title = "South Bend Quality of Life", titleWidth = 300),
  
  ## Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Department Calls", tabName = "calls", icon = icon("phone")),
      menuItem("Emergency Services", tabName = "emergency", icon = icon("building")),
      menuItem("Business Licenses", tabName = "licenses", icon = icon("briefcase")),
      menuItem("School-Park Routes", tabName = "routes", icon = icon("route"))
    )
  ),
  
  ## Dashboard Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Prevent body scrolling */
        html, body {
          height: 100%;
          overflow: hidden;
        }
        .wrapper {
          height: 100vh;
          overflow: hidden;
        }
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
          height: calc(100vh - 50px);
          overflow: hidden;
        }
        .content {
          padding: 8px;
          height: calc(100vh - 60px);
          overflow: hidden;
        }
        .tab-content {
          height: 100%;
        }
        .tab-pane {
          height: 100%;
        }
        .small-box .icon-large {
          font-size: 50px;
        }
        .info-box {
          min-height: 60px;
        }
        .small-box {
          margin-bottom: 8px;
          min-height: 80px;
        }
        .small-box h3 {
          font-size: 24px;
          margin: 0;
        }
        .small-box p {
          font-size: 12px;
        }
        .small-box .icon {
          font-size: 50px;
        }
        .box {
          margin-bottom: 8px;
        }
        .box-header {
          border-bottom: 1px solid #f4f4f4;
          padding: 6px 10px;
        }
        .box-body {
          padding: 6px;
        }
        .main-sidebar {
          height: 100vh;
        }
        /* Make tables compact */
        .table {
          margin-bottom: 0;
          font-size: 12px;
        }
        .table td, .table th {
          padding: 4px 8px;
        }
        /* Fluid row spacing */
        .row {
          margin-bottom: 0;
        }
      "))
    ),
    
    tabItems(
      ## Home Tab with KPIs
      tabItem(
        tabName = "home",
        fluidRow(
          valueBoxOutput("kpi_calls", width = 3),
          valueBoxOutput("kpi_businesses", width = 3),
          valueBoxOutput("kpi_parks", width = 3),
          valueBoxOutput("kpi_population", width = 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_facilities", width = 3),
          valueBoxOutput("kpi_schools", width = 3),
          valueBoxOutput("kpi_lights", width = 3),
          valueBoxOutput("kpi_districts", width = 3)
        ),
        fluidRow(
          box(
            title = "311 Calls by Department",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("home_calls_plot", height = "calc(100vh - 320px)")
          ),
          box(
            title = "Business License Status",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("home_license_plot", height = "calc(100vh - 320px)")
          )
        )
      ),
      
      ## Department Calls Tab
      tabItem(
        tabName = "calls",
        fluidRow(
          box(
            title = "Filter Options",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            height = "calc(100vh - 80px)",
            dateRangeInput(
              "call_dates",
              "Select Date Range:",
              start = min(phone_calls$Call_Date),
              end   = max(phone_calls$Call_Date),
              min = min(phone_calls$Call_Date),
              max = max(phone_calls$Call_Date)
            ),
            checkboxGroupInput(
              "call_months",
              "Or Select Specific Months:",
              choices = sort(unique(
                lubridate::month(phone_calls$Call_Date, label = TRUE, abbr = FALSE)
              )),
              selected = NULL
            ),
            selectInput(
              "call_departments",
              "And/Or Select Specific Departments:",
              choices = c("All Departments" = "ALL", sort(unique(phone_calls$Department))),
              selected = "ALL"
            ),
            actionButton("reset_calls", "Reset Filters", icon = icon("redo"), class = "btn-primary")
          ),
          box(
            title = "Call Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            plotOutput("call_plot", height = "calc(100vh - 120px)")
          )
        )
      ),
      
      ## Emergency Service Coverage Tab
      tabItem(
        tabName = "emergency",
        fluidRow(
          box(
            title = "Service Options",
            status = "danger",
            solidHeader = TRUE,
            width = 3,
            radioButtons(
              "service_type",
              "Select Service:",
              choices = c("Fire Station", "Police Station")
            ),
            selectInput("facility_dropdown", "Select Facility:", choices = NULL),
            sliderInput(
              "service_radius",
              "Select Radius (miles):",
              min = 0.5,
              max = 5,
              value = 2,
              step = 0.25
            ),
            hr(),
            h5("Coverage Statistics"),
            tableOutput("service_table")
          ),
          box(
            title = "Population Coverage Map",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("service_map", height = "calc(100vh - 120px)")
          )
        )
      ),
      
      ## Business Licenses Tab
      tabItem(
        tabName = "licenses",
        fluidRow(
          box(
            title = "Filter Options",
            status = "success",
            solidHeader = TRUE,
            width = 3,
            selectInput(
              "district",
              "Select City Council District:",
              choices = c("All Districts", sort(unique(businesses_indiana_districts$District)))
            ),
            uiOutput("license_type_ui"),
            actionButton("reset_filters", "Reset Filters", icon = icon("redo"), class = "btn-success"),
            hr(),
            uiOutput("license_summary_title"),
            tableOutput("license_summary")
          ),
          box(
            title = "Business License Map",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("license_map", height = "calc(100vh - 120px)")
          )
        )
      ),
      
      ## Street Lights / Routes Tab
      tabItem(
        tabName = "routes",
        fluidRow(
          box(
            title = "Route Selection",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            selectInput("school", "Select a School:", choices = school_boundaries_3857$School),
            selectInput("park", "Select a Park:", choices = NULL),
            sliderInput(
              "radius_miles",
              "Walking distance radius (miles):",
              min = 0,
              max = 2,
              value = 2,
              step = 0.1
            ),
            uiOutput("no_parks_msg"),
            hr(),
            h5("Route Statistics"),
            tableOutput("route_stats_table")
          ),
          box(
            title = "Walking Route Map",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            leafletOutput("map", height = "calc(100vh - 120px)")
          )
        )
      )
    )
  )
)

## Server
server <- function(input, output, session) {
  
  ## -----------------------------------------------------------------
  ## Home Tab - KPIs and Summary Charts
  ## -----------------------------------------------------------------
  
  output$kpi_calls <- renderValueBox({
    valueBox(
      format(total_calls, big.mark = ","),
      "Total 311 Calls",
      icon = icon("phone"),
      color = "blue"
    )
  })
  
  output$kpi_businesses <- renderValueBox({
    valueBox(
      format(total_businesses, big.mark = ","),
      "Registered Businesses",
      icon = icon("briefcase"),
      color = "green"
    )
  })
  
  output$kpi_parks <- renderValueBox({
    valueBox(
      total_parks,
      "City Parks",
      icon = icon("tree"),
      color = "olive"
    )
  })
  
  output$kpi_population <- renderValueBox({
    valueBox(
      format(total_population, big.mark = ","),
      "Total Population",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$kpi_facilities <- renderValueBox({
    valueBox(
      total_facilities,
      "Public Facilities",
      icon = icon("building"),
      color = "red"
    )
  })
  
  output$kpi_schools <- renderValueBox({
    valueBox(
      total_schools,
      "Schools",
      icon = icon("school"),
      color = "yellow"
    )
  })
  
  output$kpi_lights <- renderValueBox({
    valueBox(
      format(total_street_lights, big.mark = ","),
      "Street Lights",
      icon = icon("lightbulb"),
      color = "aqua"
    )
  })
  
  output$kpi_districts <- renderValueBox({
    valueBox(
      total_districts,
      "Council Districts",
      icon = icon("map"),
      color = "navy"
    )
  })
  
  ## Home page charts
  output$home_calls_plot <- renderPlot({
    phone_calls %>%
      count(Department, sort = TRUE) %>%
      head(8) %>%
      ggplot(aes(x = reorder(Department, n), y = n, fill = Department)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "", y = "Number of Calls") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10)) +
      scale_fill_brewer(palette = "Blues")
  })
  
  output$home_license_plot <- renderPlot({
    businesses_indiana_districts %>%
      st_drop_geometry() %>%
      count(status_group, sort = TRUE) %>%
      ggplot(aes(x = reorder(status_group, n), y = n, fill = status_group)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "", y = "Number of Businesses") +
      theme_minimal() +
      scale_fill_manual(values = c("Safe" = "#28a745", "Expired" = "#dc3545", "Other" = "#6c757d"))
  })
  
  output$home_recent_calls <- renderTable({
    phone_calls %>%
      mutate(Month = lubridate::month(Call_Date, label = TRUE, abbr = TRUE)) %>%
      count(Month, name = "Calls") %>%
      arrange(desc(Calls)) %>%
      head(5)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$home_top_calls <- renderTable({
    phone_calls %>%
      count(Called_About, sort = TRUE) %>%
      head(5) %>%
      rename("Call Type" = Called_About, "Count" = n)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  ## -----------------------------------------------------------------
  
  ## Tab 1 -  City Calls by Department
  
  ## Get user input - prioritize check boxes over date range
  selected_call_dates <- reactive({
    req(input$call_dates)
    
    if (length(input$call_months) > 0) {
      phone_calls_filtered <- phone_calls %>% filter(lubridate::month(Call_Date, label = TRUE, abbr = FALSE) %in% input$call_months)
    }
    else{
      phone_calls_filtered <- phone_calls %>% filter(Call_Date >= as.POSIXct(input$call_dates[1]) &
                                                       Call_Date <= as.POSIXct((input$call_dates[2] + 1)))
    }
    
    if (!is.null(input$call_departments) &&
        !"ALL" %in% input$call_departments) {
      phone_calls_filtered <- phone_calls_filtered %>%
        filter(Department %in% input$call_departments)
    }
    phone_calls_filtered
  })
  
  observeEvent(input$reset_calls, {
    ## Reset date to full range
    updateDateRangeInput(
      session,
      "call_dates",
      start = min(phone_calls$Call_Date),
      end   = max(phone_calls$Call_Date)
    )
    
    ## Clear month checkboxes
    updateCheckboxGroupInput(session, "call_months", selected = character(0))
    
    ## Reset department to all
    updateSelectInput(session, "call_departments", selected = "ALL")
  })

  output$call_plot <- renderPlot({
    df <- selected_call_dates()
    
    ## Top 5 called about
    top_calls <- df %>%
      count(Called_About, sort = TRUE) %>%
      head(5)
    
    ## Calls by month
    month_counts <- df %>%
      mutate(Month = lubridate::month(Call_Date, label = TRUE, abbr = TRUE)) %>%
      count(Month) %>%
      arrange(desc(n))
    
    ## Convert Month to factor in order of descending count, with most calls on top
    month_counts$Month <- factor(month_counts$Month, levels = rev(month_counts$Month))
    
    ## Top 5 departments
    top_departments <- df %>%
      count(Department, sort = TRUE) %>%
      head(5)
    
    ## Make axis consistent for each graph
    max_calls <- max(
      max(top_calls$n, na.rm = TRUE),
      max(month_counts$n, na.rm = TRUE),
      max(top_departments$n, na.rm = TRUE)
    )
    
    ## Plots
    call_plot <- ggplot(top_calls, aes(x = reorder(Called_About, n), y = n)) +
      geom_col(fill = "blue") +
      coord_flip() +
      ylim(0, max_calls) +
      labs(title = "Top Call Types", x = "", y = "Number of Calls") +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    
    month_plot <- ggplot(month_counts, aes(x = Month, y = n)) +
      geom_col(fill = "orange") +
      coord_flip() +
      ylim(0, max_calls) +
      labs(title = "Calls by Month", x = "", y = "Number of Calls") +
      theme(axis.text.y = element_text(hjust = 1))
    
    department_plot <- ggplot(top_departments, aes(x = reorder(Department, n), y = n)) +
      geom_col(fill = "forestgreen") +
      coord_flip() +
      ylim(0, max_calls) +
      labs(title = "Top Departments", x = "", y = "Number of Calls") +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    
    ## Combine plots
    call_plot / month_plot / department_plot
  })
  
  ## Top Call Types table
  output$call_type_table <- renderTable({
    df <- selected_call_dates()
    
    df %>%
      count(Called_About, sort = TRUE) %>%
      head(5) %>%
      rename(
        "Call Type" = Called_About,
        "Number of Calls" = n
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  ## Calls by month
  output$month_table <- renderTable({
    df <- selected_call_dates()
    
    df %>%
      mutate(Month = lubridate::month(Call_Date, label = TRUE, abbr = TRUE)) %>%
      count(Month) %>%
      arrange(desc(n)) %>%
      rename("Month" = Month, "Number of Calls" = n)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  ## Departments table
  output$department_table <- renderTable({
    df <- selected_call_dates()
    
    df %>%
      count(Department, sort = TRUE) %>%
      head(5) %>%
      rename("Department" = Department, "Number of Calls" = n)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  ## Duration table
  output$duration_table <- renderTable({
    df <- selected_call_dates()
    
    df %>%
      group_by(Called_About) %>% 
      summarize(n = n(), avg_call_dur = round(mean(duration_Seconds, na.rm=TRUE),0)) %>% 
      arrange(desc(n))%>% head(5) %>%
      rename("Call Type" = Called_About, "Number of Calls" = n, "Average Call Length (Seconds)" = avg_call_dur)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  
## -----------------------------------------------------------------
  
  ## Tab 2 - Emergency Service Coverage
  
  ## Get service type
  observeEvent(input$service_type, {
    req(input$service_type)
    facilities_selected_type <- facilities_3857 %>% filter(toupper(POPL_TYPE) ==
                                                             toupper(input$service_type))
    ## Update facilities based on service selection
    updateSelectInput(
      session,
      "facility_dropdown",
      choices = facilities_selected_type$POPL_NAME,
      selected = NULL
    )
  })
  
  selected_facility <- reactive({
    req(input$facility_dropdown)
    facilities_3857 %>% filter(POPL_NAME == input$facility_dropdown)
  })
  
  ## Get service area
  service_area <- reactive({
    req(selected_facility())
    radius_m <- input$service_radius * meters_per_mile
    st_buffer(selected_facility(), dist = radius_m)
  })
  
  other_facilities_in_radius <- reactive({
    req(service_area(), selected_facility(), input$service_type)
    facilities_3857 %>%
      filter(
        toupper(POPL_TYPE) == toupper(input$service_type),
        POPL_NAME != selected_facility()$POPL_NAME
      ) %>%
      st_intersection(service_area()) %>%
      nrow()
  })
  
  ## Get population served
  population_served <- reactive({
    area <- service_area()
    req(area)
    tracts <- st_intersection(census_3857, area)
    sum(tracts$A00001_1, na.rm = TRUE)
  })
  
  ## Output the map
  output$service_map <- renderLeaflet({
    facility <- selected_facility()
    area <- service_area()
    
    if (is.null(facility)) {
      leaflet() %>% addTiles(group = "Basic") %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
    } else {
      other_facilities <- facilities_3857 %>%
        filter(
          toupper(POPL_TYPE) == toupper(input$service_type),
          POPL_NAME != facility$POPL_NAME
        ) %>%
        st_intersection(area)
      leaflet() %>%
        addTiles(group = "Basic") %>%
        addMarkers(data = st_transform(facility, 4326),
                   popup = facility$POPL_NAME) %>%
        addCircleMarkers(
          data = st_transform(other_facilities, 4326),
          radius = 6,
          color = "red",
          fill = TRUE,
          fillOpacity = 1,
          popup = ~POPL_NAME) %>%
        addPolygons(
          data = st_transform(area, 4326),
          color = "blue",
          weight = 4
        )
    }
  })
  
  ## Table of metrics
  output$service_table <- renderTable({
    facility <- selected_facility()
    area <- service_area()
    population <- population_served()
    other_count <- other_facilities_in_radius()
    req(facility, area, population)
    
    ## Meters squared to square miles
    area_sq_miles <- as.numeric(st_area(area)) * meters_to_sq_miles
    pop_density <- round(population / area_sq_miles, 2)

    data.frame(
      "Facility Name" = facility$POPL_NAME,
      "Service Type" = input$service_type,
      "Radius Served (miles)" = input$service_radius,
      "Population Served" = population,
      "Population per Sq Mile" = pop_density,
      "Other Facilities within Radius" = other_count,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
## -----------------------------------------------------------------
  
  ## Tab 3 - Business License Analysis
  
  ## Allow for clicking district or selecting in dropdown
  selected_district <- reactiveVal(NULL)
  
  ## Leaflet map output
  output$license_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = districts_sf,
        color = "black",
        weight = 2,
        fillOpacity = 0.1,
        label = ~ paste("District", Num),
        layerId = ~ Num
      ) %>%
      setView(lng = -86.25,
              lat = 41.68,
              zoom = 12)
  })
  
  ## User dropdown selection
  observeEvent(input$district, {
    req(input$district)
    
    selected_district(input$district)
    
    if (input$district != "All Districts") {
      district_num <- gsub("District ", "", input$district)
      district_poly <- districts_sf %>% filter(Num == district_num)
      req(nrow(district_poly) > 0)
      bounds <- st_bbox(district_poly)
      
      leafletProxy("license_map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = district_poly,
          color = "blue",
          weight = 2,
          fillOpacity = 0.1,
          label = ~ paste("District", Num),
          group = "highlight"
        ) %>%
        fitBounds(
          lng1 = as.numeric(bounds["xmin"]),
          lat1 = as.numeric(bounds["ymin"]),
          lng2 = as.numeric(bounds["xmax"]),
          lat2 = as.numeric(bounds["ymax"])
        )
    }
  })
  
  observeEvent(input$reset_filters, {
    ## Reset district and license dropdowns
    updateSelectInput(session, "district", selected = "All Districts")
    updateSelectInput(session, "expired_business", selected = "All Expired")
    
    ## Reset reactive value
    selected_district("All Districts")
    
    ## Reset map highlight and view
    leafletProxy("license_map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = districts_sf,
        color = "black",
        weight = 2,
        fillOpacity = 0.1,
        label = ~ paste("District", Num),
        group = "highlight"
      ) %>%
      setView(lng = -86.25,
              lat = 41.68,
              zoom = 12)
  })
  
  observeEvent(input$license_map_shape_click, {
    click <- input$license_map_shape_click
    
    ## Deselect logic
    if (is.null(click$id)) {
      selected_district("All Districts")
      updateSelectInput(session, "district", selected = "All Districts")
      
      leafletProxy("license_map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = districts_sf,
          color = "black",
          weight = 2,
          fillOpacity = 0.1,
          label = ~ paste("District", Num),
          group = "highlight"
        ) %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
      return()
    }
    
    ## Clicking selected district goes back to all districs
    district_name <- paste("District", click$id)
    if (!is.null(selected_district()) &&
        selected_district() == district_name) {
      selected_district("All Districts")
      updateSelectInput(session, "district", selected = "All Districts")
      
      leafletProxy("license_map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = districts_sf,
          color = "black",
          weight = 2,
          fillOpacity = 0.1,
          label = ~ paste("District", Num),
          group = "highlight"
        ) %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
      return()
    }
    
    ## Select clicked district if not selected already
    selected_district(district_name)
    updateSelectInput(session, "district", selected = district_name)
    
    district_poly <- districts_sf %>% filter(Num == click$id)
    bounds <- st_bbox(district_poly)
    
    leafletProxy("license_map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = district_poly,
        color = "blue",
        weight = 2,
        fillOpacity = 0.1,
        label = ~ paste("District", Num),
        group = "highlight"
      ) %>%
      fitBounds(
        lng1 = as.numeric(bounds["xmin"]),
        lat1 = as.numeric(bounds["ymin"]),
        lng2 = as.numeric(bounds["xmax"]),
        lat2 = as.numeric(bounds["ymax"])
      )
  })
  
  ## Filter businesses by selected district
  district_businesses <- reactive({
    if (is.null(selected_district()) ||
        selected_district() == "All Districts") {
      businesses_indiana_districts
    } else {
      businesses_indiana_districts %>% filter(District == selected_district())
    }
  })
  
  observeEvent(selected_district(), {
    district_name <- selected_district()
    
    ## If "All Districts" remove
    if (district_name == "All Districts") {
      leafletProxy("license_map") %>%
        clearGroup("highlight") %>%
        addPolygons(
          data = districts_sf,
          color = "black",
          weight = 2,
          fillOpacity = 0.1,
          label = ~ paste("District", Num),
          group = "highlight"
        ) %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
    }
  })
  
  
  ## Get active businesses for an expired business
  closest_active_business <- reactive({
    req(selected_district())
    if (is.null(input$expired_business) ||
        input$expired_business == "")
      return(NULL)
    
    ## Filter businesses by district
    # df <- if (selected_district() == "All Districts") {
    #   businesses_indiana_districts
    # } else {
    #   businesses_indiana_districts %>% filter(District == selected_district())
    # }
    df <- district_businesses()
    
    if (input$expired_business == "All Expired") {
      ## If all expired, show all expired
      expired <- df %>% filter(status_group == "Expired") %>%
        st_make_valid() %>% st_collection_extract("POINT")
      return(lapply(1:nrow(expired), function(i)
        list(expired = expired[i, ], safe = NULL)))
    } else {
      ## Else filter by license type showing safe and expired
      expired <- df %>% filter(status_group == "Expired",
                               `License Type` == input$expired_business) %>%
        st_make_valid() %>% st_collection_extract("POINT")
      safe <- df %>% filter(status_group == "Safe",
                            `License Type` == input$expired_business) %>%
        st_make_valid() %>% st_collection_extract("POINT")
      
      if (nrow(expired) == 0 || nrow(safe) == 0)
        return(NULL)
      
      lapply(1:nrow(expired), function(i) {
        e <- expired[i, ]
        idx <- st_nearest_feature(e, safe)
        safe_nearest <- safe[idx, ]
        list(expired = e, safe = safe_nearest)
      })
    }
  })
  
  ## Show expired licenses in selected district
  output$license_type_ui <- renderUI({
    req(selected_district())
    
    ## Filter businesses in the selected district
    df <- district_businesses()
    
    ## Expired license types
    expired_types <- df %>%
      st_drop_geometry() %>%
      filter(status_group == "Expired") %>%
      pull(`License Type`) %>%
      unique() %>%
      sort()
    
    choices <- c("All Expired", expired_types)
    
    selectInput(
      "expired_business",
      "Select License to Replace:",
      choices = choices,
      selected = "All Expired"
    )
  })
  
  output$license_summary_title <- renderUI({
    district_name <- selected_district()
    if (is.null(district_name))
      district_name <- "All Districts"
    h4(paste("License Status Summary -", district_name))
  })
  
  
  ## Update map markers based on expired license selection
  observe({
    nearest <- closest_active_business()
    
    leafletProxy("license_map") %>%
      clearGroup("markers")
    
    if (!is.null(nearest)) {
      for (pair in nearest) {
        leafletProxy("license_map") %>%
          addCircleMarkers(
            data = st_transform(pair$expired, 4326),
            radius = 6,
            color = "red",
            fill = TRUE,
            fillOpacity = 0.8,
            stroke = FALSE,
            label = ~ paste("Expired:", `Business Name`, "-", `License Type`),
            group = "markers"
          )
        
        if (!is.null(pair$safe)) {
          leafletProxy("license_map") %>%
            addCircleMarkers(
              data = st_transform(pair$safe, 4326),
              radius = 6,
              color = "green",
              fill = TRUE,
              fillOpacity = 0.8,
              stroke = FALSE,
              label = ~ paste("Next Safe:", `Business Name`, "-", `License Type`),
              group = "markers"
            )
        }
      }
      
      colors <- c("red")
      labels <- c("Expired Business")
      if (any(sapply(nearest, function(x)
        ! is.null(x$safe)))) {
        colors <- c(colors, "green")
        labels <- c(labels, "Valid Business")
      }
      
      leafletProxy("license_map") %>%
        clearControls() %>%
        addLegend(colors = colors,
                  labels = labels,
                  group = "markers")
    }
  })
  
  
  ## Table of expired metrics
  output$license_summary <- renderTable({
    
    df <- district_businesses() %>% st_drop_geometry()
    
    if (nrow(df) == 0) {
      return(data.frame(Metric = NA, Value = NA))
    }
    
    ## All expired in district
    total_expired_df <- df %>% filter(status_group == "Expired")
    total_expired_n <- nrow(total_expired_df)
    
    ## CASE 1: All Expired → original summary
    if (is.null(input$expired_business) ||
        input$expired_business == "All Expired") {
      
      return(
        df %>%
          count(status_group) %>%
          mutate(percent = round(100 * n / sum(n), 2)) %>%
          rename(
            "License Status" = status_group,
            "Number of Businesses" = n,
            "Percent of Total" = percent
          )
      )
    }
    
    ## CASE 2: Specific license selected
    expired_selected_n <- total_expired_df %>%
      filter(`License Type` == input$expired_business) %>%
      nrow()
    
    pct_of_expired <- ifelse(
      total_expired_n > 0,
      round(100 * expired_selected_n / total_expired_n, 2),
      NA
    )
    
    data.frame(
      Metric = c(
        paste("Expired", input$expired_business),
        "Total Expired Businesses (District)",
        "Percent of District Expired"
      ),
      Value = c(
        as.integer(expired_selected_n),
        as.integer(total_expired_n),
        paste0(pct_of_expired, "%")
      )
    )
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  
  
## -----------------------------------------------------------------
  
  ## Tab 4 - Street lights along student walking paths
  ## Reactive based on selected school
  school_point <- reactive({
    req(input$school)
    school_boundaries_3857 %>%
      filter(School == input$school) %>%
      st_centroid()
  })
  
  ## Get parks within user-selected radius
  parks_within_radius <- reactive({
    req(school_point())
    radius_m <- input$radius_miles * meters_per_mile
    
    parks_3857 %>%
      filter(Park_Type_Kid_Friendly,
             as.numeric(st_distance(geometry, school_point())) <= radius_m)
  })
  
  ## Output error message if no parks are within walking distance
  output$no_parks_msg <- renderUI({
    if (nrow(parks_within_radius()) == 0) {
      tags$p("No kid-friendly parks within the selected walking distance.",
             style = "color:red; font-weight:bold;")
    } else {
      NULL
    }
  })
  
  ## Update park dropdown if radius slider changes
  ## Parks already update based on school selection
  observeEvent(parks_within_radius(), {
    nearby_parks <- parks_within_radius()
    
    if (nrow(nearby_parks) == 0) {
      updateSelectInput(session,
                        "park",
                        choices = character(0),
                        selected = NULL)
    } else {
      updateSelectInput(session,
                        "park",
                        choices = nearby_parks$Park_Name,
                        selected = NULL)
    }
  })
  
  
  ## Park point reactive
  park_point <- reactive({
    parks <- parks_within_radius()
    if (nrow(parks) == 0)
      return(NULL)
    
    parks %>%
      filter(Park_Name == input$park)
  })
  
  ## Walking route
  ## tryCatch for same purpose as above
  route <- reactive({
    park <- park_point()
    if (is.null(park))
      return(NULL)
    
    tryCatch({
      all_routes_nested[[input$school]][[input$park]]$route %>%
        st_transform(3857)
    }, error = function(e)
      NULL)
  })
  
  
  
  ## Buffer route with margin of error defined in QMD
  ## tryCatch in case route does not exist
  route_buffer <- reactive({
    r <- route()
    if (is.null(r))
      return(NULL)
    
    buffer_m <- 50  # fixed corridor width in meters
    st_buffer(st_make_valid(r), dist = buffer_m)
  })
  
  
  ## Street lights along route
  route_lights <- reactive({
    r <- route()
    if (is.null(r))
      return(NULL)
    
    tolerance_m <- 100  # meters from the route line
    
    street_lights_3857 %>%
      filter(as.numeric(st_distance(geometry, r)) <= tolerance_m)
  })
  
  ## Stats output table
  ## If null, display no output
  output$route_stats_table <- renderTable({
    lights <- route_lights()
    buf <- route_buffer()
    r <- route()
    
    req(lights, buf, r)
    
    ## Metrics calculation
    area_sq_miles <- as.numeric(st_area(buf)) * meters_to_sq_miles
    num_lights <- nrow(lights)
    lights_density <- round(num_lights / area_sq_miles, 2)
    distance_miles <- round(as.numeric(st_length(r)) / meters_per_mile, 2)
    total_lumens <- sum(lights$lumens_numeric, na.rm = TRUE)
    lumens_per_sq_mile <- round(total_lumens / area_sq_miles, 2)
    pct_missing <- round(100 * sum(is.na(lights$lumens_numeric)) / num_lights, 2)
    
    ## Return as table
    data.frame(
      "Route Distance (miles)" = distance_miles,
      "Buffer Area (sq miles)" = round(area_sq_miles, 2),
      "Number of Street Lights" = num_lights,
      "Lights per Sq Mile" = lights_density,
      "Total Illumination (lumens)" = total_lumens,
      "Lumens per Sq Mile" = lumens_per_sq_mile,
      "Lights without Lumens Data (%)" = pct_missing,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  ## Map output safely
  output$map <- renderLeaflet({
    lights <- route_lights()
    r <- route()
    park <- park_point()
    
    if (is.null(lights) || is.null(r) || is.null(park)) {
      leaflet() %>% addTiles(group = "Basic") %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
    } else {
      leaflet() %>%
        addTiles(group = "Basic") %>%
        addMarkers(data = st_transform(school_point(), 4326),
                   popup = school_point()$School) %>%
        addMarkers(data = st_transform(park, 4326),
                   popup = park$Park_Name) %>%
        addPolylines(
          data = st_transform(r, 4326),
          color = "blue",
          weight = 4
        ) %>%
        addCircleMarkers(
          data = st_transform(lights, 4326),
          radius = 4,
          fillColor = ~ ifelse(is.na(lumens_numeric), "red", "yellow"),
          fillOpacity = 1,
          stroke = FALSE,
          popup = ~ paste("Lumens:", lumens_numeric)
        )
    }
  })
}

## -----------------------------------------------------------------

## Run Dashboard
shinyApp(ui = ui, server = server)