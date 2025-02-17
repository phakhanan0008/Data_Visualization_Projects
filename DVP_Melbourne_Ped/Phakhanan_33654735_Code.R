# Please uncomment if any of these packages are not yet installed
#install.packages("tidyverse")
#install.packages("rjson")
#install.packages("lubridate")
#install.packages("shiny")
#install.packages("leaflet")
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("d3r")
#install.packages("plotly")
#install.packages("heatmaply")
#install.packages("htmltools")

# Import necessary libraries
library(tidyverse)
library(rjson)
library(lubridate)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(d3r)
library(plotly)
library(heatmaply)
library(htmltools)

# Load datasets
landmarks_json <- fromJSON(file = "places_of_interest.json")
pedestrian_df <- read.csv("pedestrian.csv")
air_quality_df <- read.csv("air_quality.csv")

#### Data-preprocessing Section####
### Pre-process pedestrian dataframe ###
# Convert timestamp to datetime format
pedestrian_df <- pedestrian_df %>%
  mutate(
    timestamp = ymd_hms(timestamp), # Convert timestamp to datetime format
    hour = hour(timestamp), # Extract hour from timestamp
    day_of_week = wday(timestamp, label = TRUE), # Extract day of week from timestamp
    month = month(timestamp, label = TRUE) # Extract month from timestamp
  )

# Define a function to extract season
get_season <- function(month) {
  ifelse(month %in% c(6, 7, 8), "Winter", # Months 6, 7, 8 correspond to Winter
         ifelse(month %in% c(9, 10, 11), "Spring", # Months 9, 10, 11 correspond to Spring
                ifelse(month %in% c(12, 1, 2), "Summer", "Fall"))) # Months 12, 1, 2 correspond to Summer, otherwise Fall
}

# Apply the function to get season
pedestrian_df$season <- get_season(month(pedestrian_df$timestamp))

# Assign ID to pedestrian dataframe
pedestrian_df$id <- seq_along(pedestrian_df[,1])

# Convert the timestamp column to date-time object
pedestrian_df$timestamp <- as.Date(pedestrian_df$timestamp)


### Pre-process air quality dataframe ###
# Convert the timestamp column to date-time object
air_quality_df$timestamp <- as.Date(air_quality_df$timestamp)

air_quality_df <- air_quality_df %>%
  filter(year(timestamp) >= 2023 & # Filter for years 2023 and later
           year(timestamp) <= 2024 & # Filter for years up to 2024
           pm10 < 120 & # Filter PM10 values less than 120x
           pm25 < 50) # Filter PM2.5 values less than 50

### Combining pedestrian data and air quality data ###
# Aggregate pedestrian data to find daily average across all sensors
daily_avg_pedestrian_df <- pedestrian_df %>%
  group_by(timestamp) %>%
  summarise(daily_avg_count = round(mean(total_of_directions), 0), .groups = 'drop')

# Aggregate air quality data to find daily average
daily_air_quality_df <- air_quality_df %>%
  group_by(timestamp) %>%
  summarise(
    avg_pm1 = round(mean(pm1, na.rm = TRUE), 2),
    avg_pm10 = round(mean(pm10, na.rm = TRUE), 2),
    avg_pm25 = round(mean(pm25, na.rm = TRUE), 2),
    avg_temperature = round(mean(temperature, na.rm = TRUE), 2),
    avg_humidity = round(mean(humidity, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# Merge pedestrian and air quality dataframes based on timestamp
pedestrian_air_quality_df <- merge(daily_avg_pedestrian_df, 
                                   daily_air_quality_df, 
                                   by = "timestamp")


### Combining pedestrian data and places of interest data ###
# Extract required fields and structure it as a data frame
landmarks_df <- do.call(rbind, lapply(landmarks_json, function(x) {
  data.frame(
    theme = x$theme,
    sub_theme = x$sub_theme,
    feature_name = x$feature_name,
    lat = x$lat,
    long = x$long,
    stringsAsFactors = FALSE
  )
}))

# Add ID to landmarks
landmarks_df$id <- seq_along(landmarks_df[,1])

# Process pedestrian data
pedestrian_by_loc <- pedestrian_df %>%
  group_by(timestamp, lat, long) %>%
  summarise(total_pedestrian_count = sum(total_of_directions, na.rm = TRUE), 
            .groups = 'drop')

# Convert to sf objects
pedestrian_sf <- st_as_sf(pedestrian_by_loc, coords = c("long", "lat"), crs = 4326, agr = "constant")
landmarks_sf <- st_as_sf(landmarks_df, coords = c("long", "lat"), crs = 4326, agr = "constant")

# Transform to desired CRS
landmarks_sf <- st_transform(landmarks_sf, crs = 32633)
pedestrian_sf <- st_transform(pedestrian_sf, crs = 32633)

# Create a buffer of 500 meters radius around each landmark to analyze pedestrian data within these buffers
landmarks_sf$geometry <- st_buffer(landmarks_sf$geometry, dist = 500)

# Perform a spatial join to find all pedestrian points that fall within the buffers around landmarks
joined_sf <- st_join(pedestrian_sf, landmarks_sf, join = st_within)

# Merge the joined_sf to the landmarks_df again to get latitude and longitude
pedestrian_landmarks_df <- joined_sf %>%
  left_join(landmarks_df %>% select(id, lat, long), by = "id")

# Group the data by landmark id and calculate the average total pedestrian count for each group.
average_pedestrian_loc_df <- pedestrian_landmarks_df %>%
  group_by(id) %>%
  summarise(
    avg_total_pedestrian_count = round(mean(total_pedestrian_count, na.rm = TRUE), 0),
    lat = first(lat),
    long = first(long),
    theme = first(theme),
    sub_theme = first(sub_theme),
    feature_name = first(feature_name)
  )

# Define old and new column names for renaming
oldnames = c("avg_temperature", 
             "daily_avg_count", 
             "avg_humidity", 
             "avg_pm10", 
             "avg_pm25", 
             "avg_pm1")

newnames = c("Temperature", 
             "Pedestrian Number",
             "Humidity",
             "PM10",
             "PM2.5",
             "PM1")

# Rename columns in pedestrian_air_quality_df_copy
pedestrian_air_quality_df_copy <- pedestrian_air_quality_df %>%
  select(-timestamp) %>%
  rename_at(vars(oldnames), ~ newnames)

# Compute correlation coefficients
cor.coef <- cor(pedestrian_air_quality_df_copy)

# Compute correlation p-values
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

# Compute the p-values matrix for the pedestrian_air_quality_df_copy data frame
p <- cor.test.p(pedestrian_air_quality_df_copy)

# Define a list of themes
theme_list <- c("Community Use", "Education Centre", "Health Services", "Leisure/Recreation",
                "Mixed Use", "Office", "Place Of Assembly", "Place of Worship", "Purpose Built",
                "Retail", "Specialist Residential Accommodation", "Transport", "Vacant Land")

# Filter landmarks_df to keep only rows where feature_name is in average_pedestrian_loc_df
filtered_landmarks_df <- landmarks_df %>%
  filter(feature_name %in% average_pedestrian_loc_df$feature_name)

#### Interactive Visualization Section ####
# Define UI for application
ui <- fluidPage(
  # Load the fonts
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;700&display=swap")
  ),
  # Add outer padding to the entire application
  style = "padding: 80px; font-family: 'Open Sans';",
  div(
    style = "margin-left: -30px;",
    div(
      style = "padding: 10px; display: flex; align-items: center; position: relative;",
      div(
        style = "width: 10px; height: 60px; background-color: #005A9C; position: absolute; bottom: -1px; left: -0.5px;"
      ),
      # Add the project title
      div(
        style = "padding-left: 16px; margin-top: -60px; font-size: 35px; font-weight: bolder; font-family: 'Playfair Display', serif;",
        "Analytical Dashboard of Pedestrian Behaviour and Urban Dynamics in Melbourne", 
      )
    )
  ),
  div(
    style = "border-bottom: 2px solid grey; width: 1350px; margin-left: -30px;"
  ),
  img(src = "pedcross.jpg", style = "width: 70%; display: block; margin-left: auto; margin-right: auto; padding-top: 30px;"),
  # Add an introduction, tutorial for guiding users, and introduce research question 1
  div(
    style = "font-size: 18px; padding-left: 10px; padding-right: 20px; padding-top: 45px;",
    HTML(
      "<p><strong style='font-size: 22px;'>In the bustling urban landscape of Melbourne,</strong> understanding pedestrian behavior is essential for crafting vibrant, sustainable, and livable cities. Imagine the city as a living, breathing entity, with its pulse driven by the movement of its people. Pedestrian activity, influenced by the time of day, weather conditions, and enticing places of interest, paints a vivid picture of urban life.</p>
      <p>By diving into these dynamics, we unlock powerful insights into how people navigate their surroundings. These insights guide urban planners in designing better infrastructure, ensuring our streets and public spaces are safe and accessible. They inform public health officials about how air quality and weather impact outdoor activities, helping to craft initiatives that promote healthier lifestyles. Transportation policies can be fine-tuned to match peak pedestrian times, making daily commutes smoother and more efficient.</p>
      <p>Ultimately, understanding these patterns helps us create a Melbourne that is not just functional but thriving, welcoming, and truly enjoyable for everyone.</p>
      <p style='padding-top: 45px;'><strong>This dashboard aims to answer three key research questions:</strong></p>
      <p>1. How do pedestrian counts vary over different time periods?</p>
      <p>2. How do weather conditions and air quality parameters impact pedestrian behavior?</p>
      <p>3. What is the spatial relationship between pedestrian activity and places of interest?</p>
      <p style='padding-top: 20px;'>These questions will be addressed through a series of interactive visualizations designed to provide insights into pedestrian behavior and urban dynamics in Melbourne.</p>
      <p style='padding-top: 45px; padding-bottom: 20px;'>Before we dive into the data, we have created a short tutorial video to help you navigate and interact with the dashboard. This video will guide you through the various features, including how to use dropdown lists, hover over data points, and filter visualizations.</p>
      <div style='display: flex; justify-content: center;'>
        <video width='70%' height='auto' controls>
          <source src='Phakhanan_33654735_tutorial_video.mp4' type='video/mp4'>
          Your browser does not support the video tag.
        </video>
      </div>
      <p style='padding-top: 40px;'>Now that you are familiar with how to navigate and interact with the dashboard, you’re ready to dive in and explore the data. Let’s begin with our first visualization on pedestrian counts over different time periods.</p>
      <p style='padding-top: 40px; font-size: 22px;'><strong>Research Question 1: How do pedestrian counts vary over different time periods?</strong></p>
      <p>Understanding the temporal patterns of pedestrian behavior is improtant for urban planning and resource allocation. This bar chart visualization allows you to explore how pedestrian counts fluctuate over various time periods, including daily, weekly, monthly, and seasonally.</p>
      <p>By using the <strong>dropdown menu,</strong> you can select the specific time period you are interested in to view the corresponding pedestrian count data. This feature provides flexibility in analyzing pedestrian activity at different granular levels.</p>
      ")
  ),
  # Display the bar chart
  fluidRow(
    column(
      width = 12,
      div(
        style = "padding-top: 20px; font-weight: bold; font-size: 18px; padding-left: 260px;",
        "Analysis of Pedestrian Behavior by Time Periods"
      ),
      # Dropdown menu for selecting time period
      div(
        style = "display: flex; padding: 10px; padding-left: 260px; padding-top: 10px;",
        selectInput("time_period", "Select Time Period:", 
                    choices = c("Hour", "Day of Week", "Month", "Season"),
                    selected = "Hour")
      ),
      div(
        style = "display: flex; justify-content: center; width: 1300px; padding-left: 140px;",
        plotlyOutput("barChart", width = "80%")
      )
    )
  ),
  # Explanation for the findings from the bar chart and introduce research question 2
  div(
    style = "font-size: 18px; padding-left: 10px; padding-right: 30px; padding-top: 35px;",
    HTML(
      "<p>Upon closer examination, hourly trends reveal an unexpected concentration of pedestrian activity during the early morning hours, peaking between <strong>2 AM and 7 AM</strong> with counts surpassing 600 pedestrians. In contrast, the afternoon and evening hours, particularly from <strong>1 PM to 8 PM</strong>, show notably lower counts. This nocturnal surge in pedestrian presence suggests nocturnal events or establishments that operate during these hours, such as night shifts or entertainment venues.</p>
      <p>Weekly analysis indicates a relatively stable pedestrian flow, with counts predominantly ranging from 400 to 450. <strong>Fridays and Saturdays</strong> stand out with marginally elevated counts, hinting at potential pre-weekend and weekend social patterns or events that draw larger crowds.</p>
      <p>Similarly, monthly trends maintain a consistent pedestrian count, with most months averaging between 380 and 450 pedestrians. However, <strong>October</strong> markedly diverges from this trend, recording a low count of 212 pedestrians. This warrants further investigation into whether this decline is attributed to seasonal factors, public holidays, or other events that may influence pedestrian movement.</p>
      <p>Seasonal analysis also uncovers uniformity within each category, with counts consistently around 420 pedestrians. <strong>Winter</strong> showcases the <strong>peak</strong> count with 429 pedestrians, while <strong>spring</strong> has the <strong>lowest</strong> number of average pedestrians at 414. This variation possibly reflects the influence of seasonal activities, weather conditions, or holiday periods on pedestrian movement.</p>
      <p style='padding-top: 40px; font-size: 22px;'><strong>Research Question 2: How do weather conditions and air quality parameters impact pedestrian behavior?</strong><p>
      <p>Understanding the influence of weather conditions and air quality on pedestrian behavior is crucial for urban planning, public health, and improving the overall livability of a city. This section explores the relationship between various environmental factors and pedestrian activity.</p>
      <p>The scatter plot visualization allows you to analyze how different weather conditions such as temperature and humidity, as well as air quality parameters like PM2.5, PM10, and ozone levels, affect pedestrian numbers. By selecting specific parameters from the <strong>dropdown menu,</strong> you can observe patterns and correlations.</p>
      <p>The correlation heatmap provides a comprehensive overview of the relationships between various weather and air quality parameters and pedestrian counts. By examining the heatmap, you can identify which factors have the strongest correlations with pedestrian activity. This analysis helps in understanding the extent to which environmental conditions influence people's decision to walk.</p>"
    )
  ),
  # Display the scatter plot and the correlation heat map
  fluidRow(
    style = "padding-top: 20px;",
    column(
      width = 6,
      # Add chart title
      div(
        style = "padding: 10px; font-size: 18px; font-weight: bold; padding-left: 30px;", 
          "Impact of Weather/Air Quality Parameters on Pedestrian Activity"),
      # Dropdown menu for selecting parameters
      div(
        style = "padding-left: 30px; padding-top: 15px;",
        selectInput("weather_param", "Select Parameter:", 
                    choices = c("Temperature", "Humidity", "PM1", "PM2.5", "PM10"),
                    selected = "Temperature")
      ),
      # Add the scatter plot
      div(
        style = "display: flex; flex-direction: column; align-items: center;",
        plotlyOutput("scatterPlot", width = "80%")
      )
    ),
    # Add chart title
    column(6, 
     div(style = "padding: 10px; font-size: 18px; font-weight: bold;", 
         "Correlation Between Pedestrian Counts and Weather/Air Quality Parameters"),
     # Add the correlation heat map
     div(style = "display: flex; justify-content: center; padding: 10px; padding-top: 110px; margin-left: -30px;",
         plotlyOutput("heatmap",))
    )
  ),
  # Explanation for the findings from the scatter plot and the correlation heat map
  div(
    style = "font-size: 18px; padding-left: 10px; padding-right: 30px; padding-top: 35px;",
    HTML(
      "<p>The scatter plot of temperature and pedestrian numbers reveals that a significant number of data points are concentrated in the middle-temperature range of <strong>12 to 18 degrees Celsius</strong>. As the temperature rises from the lower end, there is a noticeable increase in pedestrian count up to a certain point, after which the number of pedestrians <strong>stabilizes</strong> or slightly <strong>decreases</strong>. This indicates an optimal temperature range where pedestrian activity <strong>peaks. High temperatures</strong> may cause discomfort, leading to a <strong>decline</strong> or stabilization in pedestrian count.</p>
      <p>In contrast, the scatter plot of <strong>humidity</strong> versus pedestrian count shows a more dispersed distribution with no clear trend. Data points are scattered across the range of humidity values, suggesting that humidity may have a <strong>less direct or consistent impact</strong> on pedestrian count compared to temperature. The lack of a conspicuous increase or decrease in pedestrian count corresponding to humidity levels implies that pedestrian activity in Melbourne is relatively <strong>resilient to changes in humidity,</strong> or other factors might be influencing pedestrian behavior in addition to humidity.</p>
      <p>Furthermore, none of the <strong>air quality parameters,</strong> including <strong>PM1, PM2.5, and PM10,</strong> exhibit a strong or consistent pattern indicating a direct relationship between particulate matter concentrations and pedestrian count. While there is a cluster of points at the lower end of the PM1 scale, indicating that <strong>lower concentrations</strong> are more common, pedestrian count varies across a broad range at these lower PM1 concentrations. Additionally, lower concentrations of PM are associated with a wide range of pedestrian counts, and higher concentrations do not correlate with higher or lower pedestrian traffic, as shown in the correlation matrix. This suggests that while air quality is an important public health concern, <strong>other factors</strong> may have a more significant influence on pedestrian activity in a particular area.</p>
      ")
  ),
  # Introduce research question 3
  div(
    style = "font-size: 18px; padding-left: 10px; padding-right: 20px; padding-top: 30px;",
    HTML(
      "<p style='font-size: 22px;'><strong>Research Question 3: What is the spatial relationship between pedestrian activity and places of interest?</strong></p>
      <p>Understanding the spatial distribution of pedestrian activity in relation to various places of interest is essential for urban planning and enhancing the livability of a city. This section explores how pedestrian counts vary across different locations and how these patterns relate to nearby attractions and amenities.</p>
      <p>The Sankey diagram illustrates the number of places of interest in Melbourne by their category. This visualization shows the distribution of various types of attractions, such as parks, shopping centers, restaurants, and cultural sites. By analyzing the Sankey diagram, you can understand the relative abundance of different categories and their potential impact on pedestrian traffic.</p>
      <p>The dot density map provides a visual representation of pedestrian activity across Melbourne. Each dot on the map represents a specific location, with the size of the dot indicating the number of pedestrians. Larger dots signify higher pedestrian counts, allowing you to easily identify hotspots of activity.</p>
      <p>Different <strong>colors</strong> on the map represent various categories of places of interest, such as offices, transportations, and places of worship. By examining the map, you can discern how pedestrian traffic is distributed among these categories.</p>
      <p>The interactive features of the map allow you to click on specific categories in the legend to filter the displayed data. Additionally, the Sankey diagram illustrates the number of places of interest in Melbourne by their category. When <strong>a link</strong> in the Sankey diagram is clicked, the map will dynamically update to show only the circle markers corresponding to the selected category.</p>
      ")
  ),
  # Add chart title
  fluidRow(
    style = "padding-top: 30px;",
    div(
      style = "font-size: 18px; padding-left: 60px; padding-right: 20px; font-weight: bold;", 
      "Distribution of Places of Interest by Category in Melbourne"
    ),
    # Add the Sankey diagram
    fluidRow(
      column(
        width = 6,
        div(
          style = "display: flex; justify-content: center; padding-right: 50px;",
          plotlyOutput("sankeyDiagram", height = "400px")
        )
      ),
      # Explanation for the findings from the Sankey diagram next to the visualization
      column(
        width = 6,
        div(
          style = "padding: 10px; font-size: 16px; margin-right: 100px; margin-top: -30px;",
          HTML(
          "<div style='width: 200px; height: 30px; background-color: #63b8ff;'>
            <p style='font-size: 22px; font-weight: bold; align-items: center; text-align: center;'>Total: 149 places</p>
          </div>
          <div style='padding-top: 10px;'>
            <p><strong>&#x2022; Place Of Assembly</strong> has the highest count with 30 locations, indicating a significant number of venues such as halls, meeting places, or auditoriums that cater to public gatherings and events.</p>
            <p><strong>&#x2022; Leisure/Recreation</strong> follows closely with 23 locations, reflecting the city's emphasis on recreational spaces like parks, sports facilities, and other leisure amenities.</p>
            <p><strong>&#x2022; Place of Worship</strong> is also notable with 21 locations, showcasing the presence of various religious and spiritual centers.</p>
            <p><strong>&#x2022;</strong> Categories like <strong>Health Services</strong> and <strong>Education Centre</strong> have fewer locations, with 5 and 8 respectively, which might indicate specialized zones or clusters of such services.
  </p>
            <p><strong>&#x2022; Specialist Residential Accommodation</strong> and <strong>Vacant Land</strong> have the lowest counts with only 1 location each, indicating their rarity in the urban landscape.</p>
            </div>"
          )
        )
      )
    )
  ),
  # Display the interactive symbol map
  fluidRow(
    # Add chart title
    style = "padding-top: 30px;",
      div(style = "font-size: 18px; padding-left: 150px; padding-right: 20px; font-weight: bold;", 
          "Geographical Distribution of Daily Pedestrian Activity",
            # Add a checkbox for toggling the map cluster option
            div(style = "font-size: 14px;",
              checkboxInput("show_clusters", "Show Clusters", value = FALSE)
            )
          ),
    # Add the map
    div(
       style = "display: flex; flex-direction: column; align-items: center;",
       leafletOutput("dotMap", width = "80%"),
       # Add the reset map filter button
       actionButton("reset", "Reset Map", style = "margin-top: 10px;")
    )
  ),
  # Explanation for the findings from the map and conclusion
  div(
    style = "font-size: 18px; padding-left: 10px; padding-right: 30px; padding-top: 35px;",
    HTML(
      "<p>Upon examining the map, it becomes evident that a significant majority of the places of interest are clustered around the Central Business District (CBD) of Melbourne. Specifically, 145 out of 149 places of interest are located within this central area. This clustering highlights the CBD as the primary hub of activity and attraction within the city.</p>
      <p style='padding-top: 15px;'>The categories with the largest circles, indicating high pedestrian counts, include the following:</p>
      <p style='padding-top: 15px;'><strong>1. Place of Assembly</strong></p>
      <p><strong>Examples:</strong> State Theatre, Melbourne Exhibition Centre, Victorian Arts Centre</p>
      <p><strong>Insights:</strong> These venues are major attractors due to their capacity to host large events, performances, and exhibitions, drawing significant foot traffic from both locals and visitors.</p>
      <p style='padding-top: 15px;'><strong>2. Leisure/Recreation</strong></p>
      <p><strong>Examples:</strong> Federation Square</p>
      <p><strong>Insights:</strong> Leisure and recreational areas like Federation Square are popular destinations for relaxation, socializing, and public events, contributing to high pedestrian activity.</p>
      <p style='padding-top: 15px;'><strong>3. Place of Worship</strong></p>
      <p><strong>Examples:</strong> St Paul’s Cathedral, St Francis’s Church</p>
      <p><strong>Insights:</strong> Religious sites such as St Paul’s Cathedral attract not only regular worshippers but also tourists and visitors, making them key points of interest with considerable foot traffic.</p>
      <p style='padding-top: 15px;'><strong>4. Community Use</strong></p>
      <p><strong>Examples:</strong> State Library Victoria, NGV International</p>
      <p><strong>Insights:</strong> Community and cultural sites like the State Library and the National Gallery of Victoria serve as important cultural and educational centers, drawing diverse groups of people for various activities.</p>
      <p style='padding-top: 15px;'><strong>5. Mixed Use</strong></p>
      <p><strong>Examples:</strong> Melbourne Central, QV Village</p>
      <p><strong>Insights:</strong> Mixed-use developments combine retail, dining, entertainment, and office spaces, creating vibrant hubs of activity that attract large numbers of pedestrians throughout the day.</p>
      <p style='padding-top: 15px; font-size: 22px;'><strong>What is the data telling us?</strong></p>
      <p style='padding-top: 10px;'>Urban Planning: the concentration of places of interest in the CBD suggests a need for continuous investment in infrastructure and public amenities to support high pedestrian traffic. Enhancements in pedestrian pathways, public transport, and safety measures are crucial.</p>
      <p>Business Opportunities: Businesses looking to establish themselves in Melbourne can leverage these insights to choose locations with high foot traffic, particularly within the CBD and around major attractions.</p>
      <p>Cultural and Community Engagement: The high pedestrian counts around cultural, religious, and community sites highlight the importance of these spaces in fostering community engagement and cultural enrichment.</p>
      <p style='padding-top: 15px;'>The map provides a comprehensive overview of pedestrian activity across Melbourne, emphasizing the prominence of the CBD as the epicenter of urban life. The high pedestrian counts in categories like places of assembly, leisure/recreation, places of worship, community use, and mixed-use developments underscore the dynamic nature of these areas. By understanding these patterns, urban planners, businesses, and community leaders can make informed decisions to enhance the livability and vibrancy of Melbourne.</p>"
      ),
    img(src = 'pedblur.jpg', style = 'width: 70%; display: block; margin-left: auto; margin-right: auto; padding-top: 60px;'),
    # Add the data sources
    HTML("
        <p style='font-style: italic; text-align: center; padding-top: 30px; margin-left: 80px; margin-right: 80px;'>&quot;. . . .The intricate dance of pedestrian movement across Melbourne reveals a city vibrant with cultural, social, and economic interactions. As you explore the insights from this dashboard, consider how these patterns shape the urban experience. How might these dynamics evolve with future developments, and what can we do today to foster a more connected, sustainable, and thriving Melbourne? The answers lie not just in the data, but in the collective steps we take towards a better tomorrow.&quot;</p>
        <div style = 'padding-top: 60px;'>
          <p><strong>Data Souces: </strong></p>
          <p>1. Hourly Sensor Count of Pedestrians, 2023-2024: https://data.melbourne.vic.gov.au/explore/dataset/pedestrian-counting-system-monthly-counts-perhour/information/?sort=timestamp
</p>
          <p>2. Melbourne’s Air Quality, 2023-2024: https://data.melbourne.vic.gov.au/explore/dataset/argyle-square-air-quality/table/?sort=time
</p>
          <p>3. Melbourne’s Landmarks and Places of Interest, 2023-2024: https://discover.data.vic.gov.au/dataset/landmarks-and-places-of-interest-including-schools-theatre
s-health-services-sports-facilities-p
</p>
        </div>"
    ),
  )
)


# Define server logic for interactive visualizations
server <- function(input, output, session) {
  
  # Create a reactive value to store the selected theme
  selected_theme <- reactiveVal(NULL)
  
  # Observe events from a the Sankey diagram click event
  observeEvent(event_data("plotly_click", source = "sankey"), {
    click_data <- event_data("plotly_click", source = "sankey")
    print(click_data)
    # Check if click_data is not NULL
    if (!is.null(click_data)) {
      
      # Get the index of the clicked point and adjust for 1-based indexing
      selected_category <- click_data$pointNumber + 1
      print(selected_category)
      
      # Get the corresponding category name from the theme_list
      category_name <- theme_list[selected_category]
      print(category_name)
      
      # Update the selected theme with the chosen category name
      selected_theme(category_name)
    }
  })
  
  # Observe the reset button event
  observeEvent(input$reset, {
    # Reset the selected theme to NULL when the reset button is clicked
    selected_theme(NULL)
  })
  
  # Define a reactive expression to filter data based on the selected theme
  filtered_data <- reactive({
    
    # If no theme is selected, return the entire data frame
    if (is.null(selected_theme())) {
      average_pedestrian_loc_df
    } else {
      # Otherwise, filter the data for the selected theme
      average_pedestrian_loc_df %>% filter(theme == selected_theme())
    }
  })
  
  # Render a Plotly bar chart
  output$barChart <- renderPlotly({
    # Modify pedestrian_df to add a time_period_column based on the selected time period
    pedestrian_df <- pedestrian_df %>%
      mutate(time_period_column = case_when(
        input$time_period == "Hour" ~ as.character(hour),
        input$time_period == "Day of Week" ~ as.character(day_of_week),
        input$time_period == "Month" ~ as.character(month),
        input$time_period == "Season" ~ as.character(season)
      ))
    
    # Convert time_period_column to a factor with the appropriate levels
    if (input$time_period == "Hour") {
      pedestrian_df$time_period_column <- factor(pedestrian_df$time_period_column, 
                                                 levels = 0:23)
    } else if (input$time_period == "Day of Week") {
      pedestrian_df$time_period_column <- factor(pedestrian_df$time_period_column, 
                                                 levels = c("Sun", "Mon", "Tue", "Wed", 
                                                            "Thu", "Fri", "Sat"))
    } else if (input$time_period == "Month") {
      pedestrian_df$time_period_column <- factor(pedestrian_df$time_period_column, 
                                                 levels = month.abb)
    } else if (input$time_period == "Season") {
      pedestrian_df$time_period_column <- factor(pedestrian_df$time_period_column, 
                                                 levels = c("Winter", "Spring", 
                                                            "Summer", "Fall"))
    }
    
    # Group data by time_period_column and calculate the average pedestrian count
    avg_ped_by_time_df <- pedestrian_df %>%
      group_by(time_period_column) %>%
      summarise(avg_pedestrian_count = round(mean(total_of_directions, na.rm = TRUE)), .groups = 'drop')
    
    # Create a ggplot bar chart with an overlaid line plot
    plot_1 <- ggplot(avg_ped_by_time_df, aes(x = time_period_column, 
                                             y = avg_pedestrian_count, 
                                             text = paste("Number of Pedestrian:", 
                                                          avg_pedestrian_count))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_line(aes(group = 1), color = "red") +
      geom_point(aes(group = 1), color = "red") +
      labs(x = input$time_period, y = "Average Number of Pedestrians") +
      theme_minimal()
    
    # Convert the ggplot chart to a Plotly chart and customize hover labels
    ggplotly(plot_1, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        )
      )
  })
  
  # Render a Plotly scatter plot
  output$scatterPlot <- renderPlotly({
    
    # Select the appropriate column for the chosen weather parameter
    param_column <- switch(input$weather_param,
                           "Temperature" = "avg_temperature",
                           "Humidity" = "avg_humidity",
                           "PM1" = "avg_pm1",
                           "PM2.5" = "avg_pm25",
                           "PM10" = "avg_pm10")
    
    # Create a scatter plot with the selected weather parameter and daily average pedestrian count
    plot_2 <- ggplot(pedestrian_air_quality_df, 
                     aes_string(x = param_column, 
                                y = "daily_avg_count", 
                                text = "paste('Number of Pedestrians:', 
                                daily_avg_count, '<br>', 
                                input$weather_param, ':', 
                                get(param_column))")) +
      geom_point(color = "#499894", size = 3, alpha = 0.5) +
      labs(x = input$weather_param, y = "Average Number of Pedestrians") +
      theme_minimal()
    
    # Convert the scatter plot to a Plotly plot and customize hover labels
    ggplotly(plot_2, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        )
      )
  })
   
  # Render the correlation heat map
  # Reference: https://www.datanovia.com/en/blog/how-to-create-an-interactive-correlation-matrix-heatmap-in-r/
  output$heatmap <- renderPlotly({ 
    # Generate the heatmap plot with heatmaply_cor
    heatmap_plot <- heatmaply_cor(
      cor.coef,
      node_type = "scatter",
      point_size_mat = -log10(p)*2, 
      label_names = c("x", "y", "Correlation"),
      point_size_name = "-log10(p-value)",
      dendrogram = "none"
    )
    
    # Customize the heatmap layout
    heatmap_plot <- heatmap_plot %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        xaxis = list(
          tickangle = 90  
        )
      )
    # Return the customized heatmap plot
    heatmap_plot
  })
  
  # Return the customized heatmap plot  using specific color palettes for the map
  theme_colors <- colorFactor(palette = c("yellowgreen", "#8CD17A", "#E15759", 
                                          "steelblue1", "#FF9D9A", 
                                          "purple", "orange", "#D37295", 
                                          "darkgrey", "#D4A6C8", "#FFBE7D", 
                                          "#499894", "yellow", "#D7B5A6"),
                              domain = unique(average_pedestrian_loc_df$theme))
  
  # Return the customized heatmap plot  using specific color palettes for Sankey diagram
  theme_colors_sankey <- colorFactor(palette = c("yellowgreen", "#8CD17A", "#E15759", 
                                                 "steelblue1", "#FF9D9A", "purple", "orange", 
                                                 "#D37295", "darkgrey", "#D4A6C8", "#FFBE7D", 
                                                 "#499894", "yellow", "#D7B5A6"),
                                     domain = c("Community Use", "Education Centre", "Health Services",
                                                "Leisure/Recreation", "Mixed Use", "Office", "Place Of Assembly",
                                                "Place of Worship", "Purpose Built", "Retail",
                                                "Specialist Residential Accommodation", "Transport", "Vacant Land"))
  
  # Render the Sankey diagram
  output$sankeyDiagram <- renderPlotly({
    # Prepare data for Sankey diagram
    sankey_data <- filtered_landmarks_df %>%
      count(theme) %>%
      mutate(source = 0,
             target = as.integer(factor(theme)),
             value = n)
    
    # Define nodes for the Sankey diagram
    nodes <- data.frame(
      name = c("Places of Interest", unique(sankey_data$theme))
    )
    
    # Define colors for nodes and links based on themes
    node_colors <- c("skyblue", theme_colors_sankey(unique(sankey_data$theme)))
    link_colors <- theme_colors_sankey(unique(sankey_data$theme))
    
    # Define links for the Sankey diagram
    links <- data.frame(
      source = sankey_data$source,
      target = sankey_data$target,
      value = sankey_data$value
    )
    
    # Create the Sankey diagram using plot_ly
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(
        label = nodes$name,
        color = node_colors
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$value
      ),
      source = "sankey"
    ) %>% event_register("plotly_click")
  })

  # Render the dot map
  output$dotMap <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        ~long, ~lat,
        color = ~theme_colors(theme),
        radius = ~sqrt(avg_total_pedestrian_count)/10,
        fillOpacity = 0.5,
        label = ~lapply(
          paste("Place:", htmlEscape(feature_name), "<br>",
                "Category:", htmlEscape(theme), "<br>",
                "Average Pedestrians:", avg_total_pedestrian_count),
          HTML
        ),
        labelOptions = labelOptions(
          style = list("font-size" = "12px")
        )
      ) %>%
      addLegend("bottomright", pal = theme_colors_sankey, values = ~theme,
                title = "Category")
  })
  
  # Observe changes to show_clusters input
  observe({
    # Obtain a proxy object for dotMap and update it with filtered data
    proxy <- leafletProxy("dotMap", data = filtered_data())
    # Clear existing markers and marker clusters from the map
    proxy %>% clearMarkers() %>% clearMarkerClusters()
    
    # If show_clusters is TRUE, add marker clusters to the map
    if (input$show_clusters) {
      proxy %>% addMarkers(
        ~long, ~lat,
        clusterOptions = markerClusterOptions(),
        # Customize label content for clusters
        label = ~lapply(
          paste("Place:", htmlEscape(feature_name), "<br>",
                "Category:", htmlEscape(theme), "<br>",
                "Average Pedestrians:", avg_total_pedestrian_count),
          HTML
        ),
        labelOptions = labelOptions(
          style = list("font-size" = "12px")
        )
      )
    } else {
      # If show_clusters is FALSE, add circle markers to the map
      proxy %>% addCircleMarkers(
        ~long, ~lat,
        color = ~theme_colors(theme),
        radius = ~sqrt(avg_total_pedestrian_count)/10,
        fillOpacity = 0.5,
        # Customize label content for circle markers
        label = ~lapply(
          paste("Place:", htmlEscape(feature_name), "<br>",
                "Category:", htmlEscape(theme), "<br>",
                "Average Pedestrians:", avg_total_pedestrian_count),
          HTML
        ),
        labelOptions = labelOptions(
          style = list("font-size" = "12px")
        )
      )
    }
  })
}

# End of server logic

# Run the application 
shinyApp(ui = ui, server = server)