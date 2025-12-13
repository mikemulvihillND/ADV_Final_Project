suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(osrm))
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
          end   = max(phone_calls$Call_Date)
        ),
        checkboxGroupInput(
          "call_months",
          "Or select specific months:",
          choices = month.name,
          selected = NULL
        )
      ),
      mainPanel(
        h4("Top Call Types"),
        verbatimTextOutput("call_summary"),
        ## TODO
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
        selectInput("district", "Select City Council District:", choices = sort(unique(
          businesses$District
        )))
      ),
      mainPanel(
        h4("License Status Summary"),
        verbatimTextOutput("license_summary"),
        plotOutput("license_plot")
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
          hr(),
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
  ## City Calls by Department
  output$call_summary <- renderText({
    ## TODO
    paste("Top call types will appear here for selected dates/months.")
  })
  output$call_plot <- renderPlot({
    ## TODO
    plot(1, 1, main = "Call Frequency Plot Placeholder")
  })
  
  ## Emergency Service Coverage
  output$service_summary <- renderText({
    paste("Population coverage statistics will appear here for",
          input$service_type)
  })
  output$service_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng = -86,
                 lat = 41,
                 popup = "Placeholder Service Location")
  })
  
  ## Business License Analysis
  output$license_summary <- renderText({
    paste("Aggregated license status summary for district",
          input$district)
  })
  output$license_plot <- renderPlot({
    plot(1, 1, main = "Business License Status Plot Placeholder")
  })
  
  ## Street lights along student walking paths
  
  ## Reactive based on selected school
  school_point <- reactive({
    req(input$school)
    school_boundaries %>%
      filter(School == input$school) %>%
      st_centroid() %>%
      st_transform(3857)
  })
  
  ## Get parks within user-selected radius with error handling
  parks_within_radius <- reactive({
    req(school_point())
    radius_m <- input$radius_miles * 1609.34
    
    tryCatch({
      parks_sf %>%
        st_transform(3857) %>%
        filter(Park_Type_Kid_Friendly,
               as.numeric(st_distance(geometry, school_point())) <= radius_m)
    }, error = function(e) {
      ## Return empty sf if error occurs
      st_sf(data.frame(), geometry = st_sfc())
    })
  })
  
  output$no_parks_msg <- renderUI({
    if (nrow(parks_within_radius()) == 0) {
      tags$p("No kid-friendly parks within the selected walking distance.",
             style = "color:red; font-weight:bold;")
    } else {
      NULL
    }
  })
  
  ## Update park dropdown
  observeEvent(school_point(), {
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
  ## tryCatch to ensure app doesn't break if a selected school
  ## has no parks within selectd walking distance
  park_point <- reactive({
    parks <- parks_within_radius()
    if (nrow(parks) == 0)
      return(NULL)
    
    tryCatch({
      parks %>%
        filter(Park_Name == input$park) %>%
        st_transform(3857)
    }, error = function(e)
      NULL)
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
  ## tryCatch as above in case route does not exist
  route_buffer <- reactive({
    r <- route()
    if (is.null(r))
      return(NULL)
    
    tryCatch({
      st_buffer(r, dist = route_buffer_meters)
    }, error = function(e)
      NULL)
  })
  
  ## Street lights along route
  ## tryCatch to ensure route exists
  route_lights <- reactive({
    buf <- route_buffer()
    if (is.null(buf))
      return(NULL)
    
    tryCatch({
      street_lights_sf %>%
        st_transform(3857) %>%
        st_intersection(buf)
    }, error = function(e)
      NULL)
  })
  
  ## Stats output
  ## If
  output$route_stats <- renderText({
    lights <- route_lights()
    buf <- route_buffer()
    r <- route()
    
    if (is.null(lights) || is.null(buf) || is.null(r)) {
      return()
    }
    
    ## Metrics calculation
    area_sq_miles <- as.numeric(st_area(buf)) * 3.861e-7
    num_lights <- nrow(lights)
    lights_density <- num_lights / area_sq_miles
    distance_miles <- as.numeric(st_length(r)) / 1609.34
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
      leaflet() %>% addTiles(group="Basic") %>%
        setView(lng = -86.25,
                lat = 41.68,
                zoom = 12)
    } else {
      leaflet() %>%
        addTiles(group="Basic") %>%
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

## Run Dashboard
shinyApp(ui = ui, server = server)
