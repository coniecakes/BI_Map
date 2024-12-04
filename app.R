library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(readr)

full_data <- read_csv("full_data.csv")

# State centroids for mapping
state_coords <- tibble::tibble(
  state = c(
    "Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
    "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
    "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
    "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
    "Wyoming"
  ),
  lat = c(
    32.8067, 34.0489, 34.7465, 36.7783, 39.5501, 41.6032, 38.9108, 
    27.9944, 32.1656, 44.0682, 40.6331, 40.2672, 41.878, 39.0119, 37.8393, 
    30.9843, 45.2538, 39.0458, 42.4072, 44.3148, 46.7296, 32.3547, 
    37.9643, 46.8797, 41.4925, 38.8026, 43.1939, 40.0583, 34.5199, 
    40.7128, 35.7596, 47.5515, 40.4173, 35.0078, 43.8041, 
    41.2033, 41.5801, 33.8361, 43.9695, 35.5175, 
    31.9686, 39.3209, 44.5588, 37.4316, 47.7511, 38.5976, 43.7844, 
    43.076
  ),
  lon = c(
    -86.7911, -111.0937, -92.2896, -119.4179, -105.7821, -73.0877, -75.5277, 
    -81.7603, -82.9001, -114.742, -89.3985, -86.1349, -93.0977, -98.4842, -84.2700, 
    -91.9623, -69.4455, -76.6413, -71.3824, -85.6024, -93.2432, -89.3985, 
    -91.8318, -110.3626, -99.9018, -116.4194, -71.5724, -74.4057, -105.8701, 
    -74.0060, -79.0193, -101.002, -82.9071, -97.0929, -120.5542, 
    -77.1945, -71.4774, -81.1637, -100.2263, -86.5804, 
    -99.9018, -111.0937, -72.5778, -78.6569, -120.7401, -80.4549, -89.5900, 
    -107.292
  )
)

# Filter the data for valid states and add coordinates
fullData1 <- fullData %>%
  mutate(year = format(as.Date(create.date, format = "%Y-%m-%d"), "%Y")) %>%
  filter(year > 2012) %>%
  filter(state %in% state_coords$state)

# Aggregate data for each stage by year
funnel_data_yearly2 <- fullData1 %>%
  group_by(year, state) %>%
  summarise(
    Total_Leads = n(),
    Applications_Started = sum(app.start, na.rm = TRUE),
    Applications_Submitted = sum(app.submit, na.rm = TRUE),
    Applications_Completed = sum(app.complete, na.rm = TRUE),
    Admissions = sum(app.admit, na.rm = TRUE),
    Enrollments = sum(app.enroll, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(Total_Leads, Applications_Started, Applications_Submitted, 
             Applications_Completed, Admissions, Enrollments),
    names_to = "Stage",
    values_to = "Leads"  # Rename to Leads here
  )

# Prepare data for mapping
funnel_data_map <- funnel_data_yearly2 %>%
  left_join(state_coords, by = "state") %>%
  filter(!state %in% c("Hawaii", "Alaska")) %>%
  mutate(year = as.numeric(year))

# Define a color palette for the stages
stage_colors <- colorFactor(
  palette = colorRampPalette(c("red", "blue", "green", "orange", "purple", "brown"))(
    length(unique(funnel_data_map$Stage))
  ),
  domain = unique(funnel_data_map$Stage)
)
# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Map: Leads by Stage per State"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year", "Select Year:",
        min = min(funnel_data_map$year, na.rm = TRUE),
        max = max(funnel_data_map$year, na.rm = TRUE),
        value = min(funnel_data_map$year, na.rm = TRUE),
        step = 1,
        animate = animationOptions(interval = 1000, loop = TRUE),  # Enable play button
        sep = ""
      ),
      checkboxGroupInput(
        "stages", "Select Stages:",
        choices = unique(funnel_data_map$Stage),
        selected = unique(funnel_data_map$Stage)
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # Filter data based on user input
    filtered_data <- funnel_data_map %>%
      filter(year == input$year, Stage %in% input$stages)
    
    # Create the map
    leaflet(filtered_data) %>%
      addTiles() %>%  # Default OpenStreetMap tiles
      addCircleMarkers(
        lat = ~lat, lng = ~lon,
        color = ~stage_colors(Stage),
        radius = ~sqrt(Leads) * 2,
        popup = ~paste0("State: ", state, "<br>",
                        "Stage: ", Stage, "<br>",
                        "Leads: ", Leads)
      ) %>%
      addLayersControl(
        overlayGroups = unique(filtered_data$Stage),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# Run the App
shinyApp(ui, server)
