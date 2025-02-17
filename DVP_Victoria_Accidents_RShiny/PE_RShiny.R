# Please uncomment if any of these packages are not yet installed
#install.packages(tidyverse)
#install.packages(shiny)
#install.packages(ggplot2)
#install.packages(leaflet)

# Load required libraries
library(tidyverse)
library(shiny)
library(ggplot2)
library(leaflet)

# Load the dataset
accident_df <- read_csv("Victoria_Accident_Data_FIT5147S12024PE2v2.csv", show_col_types = FALSE)

## Pre-process the data ##
# Extract hour from ACCIDENT_TIME and remove the leading "0"s from the SPEED_ZONE column
accident_df <- accident_df %>%
  mutate(
    HOUR = as.integer(substr(ACCIDENT_TIME, 1, 2)),
    SPEED_ZONE = as.character(SPEED_ZONE), # Convert SPEED_Zone column to character
    SPEED_ZONE = sub("^0+", "", SPEED_ZONE), # Remove leading "0"s
    LIGHT_CONDITION_DESC = recode(LIGHT_CONDITION_DESC, `Unk.` = "Unknown") # Recode "Unk." to "Unknown"
  ) %>%
  mutate(
    # Sort the SPEED_ZONE column in ascending order
    SPEED_ZONE = factor(SPEED_ZONE, levels = sort(unique(as.numeric(SPEED_ZONE))))
  ) %>%
  mutate(
    # Replace '999' with 'Unknown' in SPEED_ZONE column
    SPEED_ZONE = fct_recode(SPEED_ZONE, "Unknown" = "999")
  )

# Aggregate data to find the number of accidents by SPEED_ZONE and LIGHT_CONDITION_DESC columns
accident_by_speed_light <- accident_df %>%
  group_by(SPEED_ZONE, LIGHT_CONDITION_DESC) %>%
  summarise(Accidents = n(), .groups = 'drop')

# Identify top 4 speed zones based on the number of accidents
top4_speed_zone <- accident_df %>%
  group_by(SPEED_ZONE,) %>%
  summarise(Accidents = n(), .groups = 'drop') %>%
  arrange(desc(Accidents)) %>% 
  head(4)

# Filter the dataset for accidents occurring in the top 4 SPEED_ZONEs
top4_speed_zone_list <- unique(top4_speed_zone$SPEED_ZONE)

# Filter data for only top 4 speed zones
filtered_df <- accident_df %>%
  filter(SPEED_ZONE %in% top4_speed_zone_list) %>%
  group_by(SPEED_ZONE, HOUR) %>%
  summarise(Accidents = n(), .groups = 'drop')

## Perform data visualization ##

# Define the UI
ui <- fixedPage(
  # App title
  div(
    style = "padding: 10px; font-size: 30px; font-weight: bold",
    "Analytical Dashboard of Road Accidents in Victoria"
  ),
  fixedRow(
    # Column for the VIS 1
    column(6, 
           div(style = "padding: 10px; font-size: 14px; font-weight: bold", 
               "Accidents by Light Condition and Speed Zone"),
           plotOutput("vis_1"), 
           # VIS 1 description and interpretation
           div(style = "padding-top: 10px; font-size: 14px",
               "This bar chart visualizes the number of road accidents that occurred under various lighting conditions across different speed zones. The x-axis categorizes the speed zones, ranging from 40 to 110 km/h, including special categories such as 777 and 888, as well as the unknown speed zone. The y-axis indicates the count of accidents. Each bar is segmented by color to reflect distinct lighting conditions. A significant observation is the concentration of accidents within the 100 km/h speed zone during daylight, exceeding 300 incidents, which is notably higher compared to other zones and conditions.")
    ),
    # Column for the VIS 2
    column(6, 
           div(style = "padding: 10px; font-size: 14px; font-weight: bold", 
               "Hourly Accidents in Top 4 Speed Zones"),
           plotOutput("vis_2"), 
           # VIS 2 description and interpretation
           div(style = "padding-top: 10px; font-size: 14px",
               "This line chart visualizes the hourly distribution of road accidents within the top 4 speed zones with the highest number of incidents. The x-axis indicates the hour of the day, from 0 to 23 hours, while the y-axis shows the number of accidents. Each of the four lines depicts the frequency of accidents with in different speed zones. From this visualization, we can observe trends and patterns about when accidents are more likely to happen in these speed zones. For example, there appears to be a peak in accidents in the 100 km/h speed zone around midday. Each speed zone has its own trend, which can provide insights into traffic conditions.")
    )
  ),
  # Interactive map section
  div(
    style = "padding-top: 30px; padding-bottom: 10px; font-size: 14px; font-weight: bold",
    "Accident by Locations and Light Condition Map"
  ),
  div(
    id = "map-container",
    # Styling for the map container
    tags$style(type = "text/css", "#map-container { position: relative; }"),
    leafletOutput("accident_map", width = "100%", height = "600px"),
    div(
      id = "filters",
      style = "position: absolute; top: 80px; left: 20px; z-index: 400; background-color: white; padding: 10px; border-radius: 8px;",
      checkboxGroupInput("daynightFilter", "Filter by Day/Night:",
                         choices = c("Day", "Dusk/Dawn", "Night"),
                         selected = c("Day", "Dusk/Dawn", "Night"))
    )
  ),
  # Map description and interpretation
  div(
    style = "padding-top: 20px; font-size: 14px",
      "This interactive map provides a visualization of road accidents based on light conditions of the day. The accidents are marked with color-coded circle markers: orange indicates daylight, purple signifies dusk or dawn, and navy represents nighttime. The filter panel on the top left enables users to selectively display accidents according to these light conditions by selecting or deselecting the options in the checklist. User can also get relevant information regarding each accident, such as the accident date and severity rank, by hovering on the circle markers. The map represents a geographic overview of accidents, with denser clusters of markers suggesting areas of higher accident frequency and the larger size of each marker reflects the higher severity of the accident. Predominantly, accidents are shown to occur during daylight, which might imply higher traffic volumes rather than lower light conditions as a more significant factor in accident rates. The variation in marker size across the map indicates that accident severity is not confined to specific times of day or light conditions."),
  div(
    style = "padding-top: 10px; ; padding-bottom: 10px; font-size: 14px",
    "Data Source: https://discover.data.vic.gov.au/dataset/victoria-road-crash-data"
  )
)

# Define the server
server <- function(input, output) {
  
  # Define the VIS 1
  output$vis_1 <- renderPlot({
    ggplot(data = accident_by_speed_light, aes(x = SPEED_ZONE, y = Accidents, fill = LIGHT_CONDITION_DESC)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(x = "Speed Zone (km/h)",
           y = "Number of Accidents",
           fill = "Light Condition") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Define the VIS 2
  output$vis_2 <- renderPlot({
    ggplot(data = filtered_df, aes(x = HOUR, y = Accidents, color = SPEED_ZONE)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(x = "Hour of the Day",
           y = "Number of Accidents",
           color = "Speed Zone (km/h)") +
      scale_x_continuous(breaks = seq(0, 23, by = 3))
  })
  
  # Define reactive expression to return filtered data based on day/night input value
  daynight_filter_data <- reactive({
    # Ensure the filter input is available
    req(input$daynightFilter) 
    accident_df %>%
      mutate(daynight = case_when(
        LIGHT_CONDITION_DESC == "Day" ~ "Day",
        LIGHT_CONDITION_DESC == "Dusk/Dawn" ~ "Dusk/Dawn",
        TRUE ~ "Night"
      )) %>%
      filter(daynight %in% input$daynightFilter)  # Filter based on selected day/night values
  })
  
  # Define the reactive map
  output$accident_map <- renderLeaflet({
    # Get day/night filter data
    data <- daynight_filter_data()
    
    # Define color palette for different light conditions
    palette <- colorFactor(c("orange", "purple", "navy"), domain = c("Day", "Dusk/Dawn", "Night"))
    
    # Plot the leaflet map
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        color = ~palette(daynight),
        radius = ~1/SEVERITY_RANK * 8, # Adjust the scale for better visibility and to avoid overlapping
        weight = 1,
        opacity = 0.65,
        fillOpacity = 0.5, # Adjust the opacity for better visibility for overlapping circle markers
        # Provide a tooltip for each circle marker on hover over
        label = ~paste(
          "Date: ", ACCIDENT_DATE, 
          ", Type: ", ACCIDENT_TYPE_DESC, 
          ", Severity Rank", SEVERITY_RANK,
          ", Light Condition: ", LIGHT_CONDITION_DESC, 
          ", Road Geometry: ", ROAD_GEOMETRY_DESC, 
          ", Speed Zone: ", SPEED_ZONE
        )
      ) %>%
      # Provide the color legend
      addLegend("bottomright", pal = palette, values = ~daynight,
                title = "Day/Night")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
