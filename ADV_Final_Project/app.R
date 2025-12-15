suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(osrm))
suppressPackageStartupMessages(library(patchwork))
options(warn = -1)

## Overall fluid page
ui <- fluidPage(
  titlePanel("South Bend Quality of Life Dashboard"),
  
  tabsetPanel(
    ## City Calls by Department
    tabPanel("Department Calls", sidebarLayout(
      sidebarPanel(
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
          ## Filter selections by months present in the data frame (no June/July)
          choices = sort(unique(
            lubridate::month(phone_calls$Call_Date, label = TRUE, abbr = FALSE)
          )),
          selected = NULL
        )
      ),
      mainPanel(
        h4("Top Call Types"),
        verbatimTextOutput("call_summary"),
        plotOutput("call_plot")
      )
    )),
    
    ## Emergency Service Coverage
    tabPanel(
      "Emergency Service Coverage",
      sidebarLayout(
        sidebarPanel(
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
          )
        ),
        mainPanel(
          h4("Population Coverage"),
          verbatimTextOutput("service_summary"),
          leafletOutput("service_map", height = 600)
        )
      )
    ),
    
    ## Business License Analysis
    tabPanel("Business Licenses", sidebarLayout(
      sidebarPanel(
        selectInput("district", "Select City Council District:", choices = sort(
          unique(businesses_indiana_districts$District)
        )),
        uiOutput("license_type_ui")
      ),
      mainPanel(
        h4("License Status Summary"),
        leafletOutput("license_map"),
        plotOutput("license_plot"),
        tableOutput("license_summary")
        
      )
    )),
    
    ## Street lights along student walking paths
    tabPanel(
      "Street Lights between Schools and Parks",
      sidebarLayout(
        sidebarPanel(
          selectInput("school", "Select a School:", choices = school_boundaries$School),
          ## Parks will dynamically update depending on school selected
          ## and the walking radius
          selectInput("park", "Select a Park:", choices = NULL),
          sliderInput(
            "radius_miles",
            "Walking distance radius (miles):",
            min = 0,
            max = 2,
            value = 2,
            step = 0.1
          ),
          h4("Route Statistics"),
          verbatimTextOutput("route_stats"),
          uiOutput("no_parks_msg")
        ),
        mainPanel(leafletOutput("map", height = 600))
      )
    )
  )
)

## Server
server <- function(input, output, session) {
  
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
                                                       Call_Date <= as.POSIXct((input$call_dates[2])))
    }
    phone_calls_filtered
  })
  
  ## Output user filtered top calls
  output$call_summary <- renderText({
    df <- selected_call_dates()
    ## Get the top 5
    called_about_count <- df %>% count(Called_About, sort = TRUE) %>% head(5)
    department_count <- df %>% count(Department, sort = TRUE) %>% head(5)
    
    ## Output the text
    paste0(
      "Top Topics Called About:\n",
      paste0(
        " - ",
        called_about_count$Called_About,
        ": ",
        called_about_count$n,
        collapse = "\n"
      ),
      "\n\nTop Departments Called:\n",
      paste0(
        " - ",
        department_count$Department,
        ": ",
        department_count$n,
        collapse = "\n"
      )
    )
    
  })
  
  ## Graphical output
  output$call_plot <- renderPlot({
    df <- selected_call_dates()
    top_calls <- df %>%
      count(Called_About, sort = TRUE) %>%
      head(5)
    
    top_departments <- df %>%
      count(Department, sort = TRUE) %>%
      head(5)
    
    call_plot <- ggplot(top_calls, aes(x = reorder(Called_About, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top Call Types", x = "", y = "Number of Calls") +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    department_plot <- ggplot(top_departments, aes(x = reorder(Department, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top Departments", x = "", y = "Number of Calls") +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    
    ## Use patchwork library to plot them together
    call_plot / department_plot
  })
  ## -----------------------------------------------------------------
  ## Tab 2 - Emergency Service Coverage
  
  observeEvent(input$service_type, {
    req(input$service_type)
    facilities_selected_type <- facilities_3857 %>% filter(toupper(POPL_TYPE) ==
                                                             toupper(input$service_type))
    
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
  
  service_area <- reactive({
    req(selected_facility())
    radius_m <- input$service_radius * meters_to_miles
    st_buffer(selected_facility(), dist = radius_m)
  })
  
  population_served <- reactive({
    area <- service_area()
    req(area)
    tracts <- st_intersection(census_3857, area)
    sum(tracts$A00001_1, na.rm = TRUE)
  })
  output$service_summary <- renderText({
    facility <- selected_facility()
    population <- population_served()
    req(facility, population)
    
    paste0(
      "Facility: ",
      facility$POPL_NAME,
      "\n",
      "Service Type: ",
      input$service_type,
      "\n",
      "Radius Served: ",
      input$service_radius,
      " miles\n",
      "Population Served within Radius: ",
      population
    )
  })
  output$service_map <- renderLeaflet({
    facility <- selected_facility()
    area <- service_area()
    
    if (is.null(facility)) {
      leaflet() %>% addTiles(group = "Basic") %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
    } else {
      leaflet() %>%
        addTiles(group = "Basic") %>%
        addMarkers(data = st_transform(facility, 4326),
                   popup = facility$POPL_NAME) %>%
        addPolygons(
          data = st_transform(area, 4326),
          color = "blue",
          weight = 4
        )
    }
  })
  
  ## -----------------------------------------------------------------
  ## Tab 3 - Business License Analysis
  
  ## Get user selected district
  district_businesses <- reactive({
    businesses_indiana_districts %>% filter(District == input$district)
  })
  
  ## Get active businesses for an expired business
  closest_active_business <- reactive({
    req(input$district, input$expired_business)
    
    expired <- businesses_indiana_districts %>% 
      filter(District == input$district,
             status_group == "Expired",
             `License Type` == input$expired_business) %>%
      st_make_valid() %>% 
      st_collection_extract("POINT")
    
    safe <- businesses_indiana_districts %>% 
      filter(District == input$district,
             status_group == "Safe",
             `License Type` == input$expired_business) %>%
      st_make_valid() %>% 
      st_collection_extract("POINT")
    
    ## If empty break out
    if(nrow(expired) == 0 | nrow(safe) == 0) return(NULL)
    
    ## Find nearest safe businesses for expired business
    nearest_list <- lapply(1:nrow(expired), function(i) {
      e <- expired[i, ]
      idx <- st_nearest_feature(e, safe)
      safe_nearest <- safe[idx, ]
      list(expired = e, safe = safe_nearest)
    })
    
    nearest_list
  })
  
  
  ## Let the expired license list update based on user district selection
  output$license_type_ui <- renderUI({
    req(input$district)
    
    ## Get all expired licenses in a district
    expired_types <- businesses_indiana_districts %>%
      st_drop_geometry() %>%
      filter(District == input$district, status_group == "Expired") %>%
      pull(`License Type`) %>%
      unique() %>%
      sort()
    
    selectInput("expired_business", "Select License to Replace:", choices = expired_types)
  })
  

  ## Map of licenses
  output$license_map <- renderLeaflet({
    nearest <- closest_active_business()
    
    if (is.null(nearest)) {
      leaflet() %>% addTiles(group = "Basic") %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
    } else {
      ## Map creation
      m <- leaflet() %>%
        addTiles(group = "Basic")
      
      ## Loop over pairs to plot
      for (pair in nearest) {
        m <- m %>%
          addCircleMarkers(
            data = st_transform(pair$expired, 4326),
            radius = 6,
            color = "red",
            fill = TRUE,
            fillOpacity = 0.8,
            stroke = FALSE,
            label = ~ paste("Expired:", `Business Name`, "-", `License Type`)
          ) %>%
          addCircleMarkers(
            data = st_transform(pair$safe, 4326),
            radius = 6,
            color = "green",
            fill = TRUE,
            fillOpacity = 0.8,
            stroke = FALSE,
            label = ~ paste("Next Safe:", `Business Name`, "-", `License Type`)
          )
      }

      m %>% addLegend(
        colors = c("red", "green"),
        labels = c("Expired Business", "Valid Business")
      )
    }
  })
  
  ## Table of expired metrics
  output$license_summary <- renderTable({
    district_businesses() %>% st_drop_geometry() %>% count(status_group) %>% mutate(percent =
                                                                                      round(100 * n / sum(n), 2))
  })
  output$license_plot <- renderPlot({
    df <- district_businesses() %>%
      st_drop_geometry() %>%
      count(status_group) %>%
      complete(status_group = c("Safe", "Expired", "Pending"),
               fill = list(n = 0)) %>%
      mutate(percent = round(100 * n / sum(n), 2))
    
    ggplot(df, aes(x = status_group, y = n, fill = status_group)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.5) +
      scale_fill_manual(values = c(
        "Safe" = "green",
        "Expired" = "red",
        "Pending" = "yellow"
      )) +
      labs(
        title = paste("Business License Status –", input$district),
        x = "Status Group",
        y = "Number of Businesses"
      ) +
      theme(legend.position = "none")
  })
  
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
    radius_m <- input$radius_miles * meters_to_miles
    
    parks_3857 %>%
      filter(Park_Type_Kid_Friendly,
             as.numeric(st_distance(geometry, school_point())) <= radius_m)
  })
  
  
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
      osrmRoute(src = st_transform(school_point(), 4326),
                dst = st_transform(park, 4326)) %>%
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
    
    tryCatch({
      st_buffer(st_make_valid(r), dist = route_buffer_meters)
    }, error = function(e)
      NULL)
    
  })
  
  ## Street lights along route
  route_lights <- reactive({
    buf <- route_buffer()
    if (is.null(buf))
      return(NULL)
    
    street_lights_3857 %>%
      st_intersection(buf)
  })
  
  
  ## Stats output
  ## If null, display no output
  output$route_stats <- renderText({
    lights <- route_lights()
    buf <- route_buffer()
    r <- route()
    
    if (is.null(lights) || is.null(buf) || is.null(r)) {
      return()
    }
    
    ## Metrics calculation
    area_sq_miles <- as.numeric(st_area(buf)) * meters_to_sq_miles
    num_lights <- nrow(lights)
    lights_density <- num_lights / area_sq_miles
    distance_miles <- as.numeric(st_length(r)) / meters_to_miles
    total_lumens <- sum(lights$lumens_numeric, na.rm = TRUE)
    lumens_per_sq_mile <- total_lumens / area_sq_miles
    pct_missing <- sum(is.na(lights$lumens_numeric)) / num_lights
    
    ## Display metrics
    paste0(
      "Route distance: ",
      round(distance_miles, 2),
      " miles\n",
      "Buffer area: ",
      round(area_sq_miles, 2),
      " sq miles\n",
      "Number of street lights: ",
      num_lights,
      "\n",
      "Lights per sq mile: ",
      round(lights_density, 2),
      "\n",
      "Total illumination: ",
      total_lumens,
      " lumens\n",
      "Lumens per sq. mile: ",
      round(lumens_per_sq_mile, 2),
      "\n",
      "Lights without lumens data: ",
      round(pct_missing, 2) * 100,
      "%"
    )
  })
  
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
