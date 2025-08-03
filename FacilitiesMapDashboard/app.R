# Load required libraries
library(readxl)
library(dplyr)
library(leaflet)
library(shiny)
library(leaflet.extras)
library(RColorBrewer)
library(shinycssloaders)
library(stringr)

# Load the data
file <- "FindTreament_Facility.xlsx"
facilities <- read_excel(file, sheet = "Facilities with service detail")

# Clean and classify data
facilities_clean <- facilities %>%
  select(name1, street1, city, county, state, zip, phone, latitude, longitude, type_facility) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    County = str_to_title(str_trim(county)),
    City = str_to_title(str_trim(city)),
    FacilityType = case_when(
      type_facility == "BUPREN" ~ "Buprenorphine Provider",
      type_facility == "HRSA" ~ "HRSA Facility",
      type_facility == "OTP" ~ "Opioid Treatment Program",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  rename(
    Name = name1,
    Street = street1,
    State = state,
    ZIP = zip,
    Phone = phone
  )

# Define NJ map bounds
nj_bounds <- list(south = 38.8, north = 41.4, west = -75.6, east = -73.8)

# UI with Reconnect Gaming theme
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f9f9f9;
        color: #111;
        font-family: 'Segoe UI', sans-serif;
      }
      .title-text {
        color: #693699;
        font-size: 28px;
        font-weight: bold;
        margin-top: 20px;
      }
      .desc-text {
        font-size: 16px;
        margin-bottom: 20px;
      }
      .well {
        background-color: #fff;
        border-left: 5px solid #693699;
      }
      .selectize-input {
        border-color: #693699 !important;
      }
      .leaflet-container {
        border: 2px solid #693699;
        border-radius: 6px;
      }
    "))
  ),
  
  div(class = "title-text", "NJ Addiction & Mental Health Services Map"),
  div(class = "desc-text", "Use the filters below to explore addiction and mental health treatment facilities in New Jersey by location and service type."),
  
  div(class = "desc-text", tags$strong("Facility Type Definitions:")),
  tags$ul(
    tags$li(tags$b("Buprenorphine Provider"), ": Offers medication-assisted treatment using buprenorphine."),
    tags$li(tags$b("HRSA Facility"), ": Federally funded health centers."),
    tags$li(tags$b("Opioid Treatment Program"), ": Certified programs offering methadone and related care.")
  ),
  
  fluidRow(
    column(3,
           wellPanel(
             selectInput("facilityType", "Facility Type:",
                         choices = c("All", sort(unique(facilities_clean$FacilityType))),
                         selected = "All"),
             selectInput("county", "County:",
                         choices = c("All", sort(unique(facilities_clean$County))),
                         selected = "All"),
             selectInput("city", "City/Town:",
                         choices = c("All", sort(unique(facilities_clean$City))),
                         selected = "All"),
             textInput("search", "Search by Facility or City Name:", placeholder = "Type keyword..."),
             br(),
             textOutput("summary")
           )
    ),
    column(9,
           withSpinner(leafletOutput("map", height = "75vh"))
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- facilities_clean
    
    # Apply dropdown filters
    if (input$facilityType != "All") {
      data <- data %>% filter(FacilityType == input$facilityType)
    }
    if (input$county != "All") {
      data <- data %>% filter(County == input$county)
    }
    if (input$city != "All") {
      data <- data %>% filter(City == input$city)
    }
    
    # Apply search keyword
    if (input$search != "") {
      keyword <- tolower(input$search)
      data <- data %>%
        filter(grepl(keyword, tolower(Name)) | grepl(keyword, tolower(City)))
    }
    
    data
  })
  
  output$summary <- renderText({
    paste(nrow(filtered_data()), "facilities match your filters.")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = nj_bounds$west, lat1 = nj_bounds$south,
                lng2 = nj_bounds$east, lat2 = nj_bounds$north)
  })
  
  observe({
    data <- filtered_data()
    types_present <- unique(data$FacilityType)
    
    pal <- colorFactor(
      palette = RColorBrewer::brewer.pal(max(3, length(types_present)), "Set1"),
      domain = types_present
    )
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0("<strong>", Name, "</strong><br>",
                        Street, ", ", City, ", ", State, " ", ZIP, "<br>",
                        "Phone: ", Phone, "<br>",
                        "Facility Type: ", FacilityType),
        radius = 6,
        fillColor = ~pal(FacilityType),
        color = "#111111", # black border
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 1
      ) %>%
      addLegend("bottomright", pal = pal, values = data$FacilityType,
                title = "Facility Type", opacity = 1)
  })
}

# Run app
shinyApp(ui, server)
