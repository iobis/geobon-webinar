source("lib.R")

datasetid <- "dc39bfb5-78df-4919-b62f-c0d18782a29a"
dggs_map <- dgconstruct(projection = "ISEA", topology = "HEXAGON", res = 9)
dggs_timeseries <- dgconstruct(projection = "ISEA", topology = "HEXAGON", res = 7)
min_years <- 7

occurrences <- occurrence(datasetid = datasetid, mof = TRUE, event = FALSE)

occurrence_mof <- measurements(occurrences, fields = c("id", "decimalLongitude", "decimalLatitude", "scientificName", "date_year", "date_mid", "species", "genus", "family", "order", "class", "superclass")) %>%
  enhance_mof() %>%
  mutate(measurementValue = as.numeric(measurementValue)) %>%
  filter(!is.na(measurementValue)) %>%
  group_by(date, date_year, month, decimalLongitude, decimalLatitude, scientificName, species, genus, family, measurementType, measurementValue, measurementUnit) %>%
  summarize(records = n())

occurrence_mof_map_biotic <- occurrence_mof %>%
  add_cell(dggs_map) %>%
  filter(measurementType %in% c("Shoot biomass", "Shoot density", "Rhizome biomass", "Cover (%)")) %>%
  group_by(scientificName, date_year, cell, measurementType) %>%
  summarize(measurementValue = mean(measurementValue))

occurrence_mof_timeseries_biotic <- occurrence_mof %>%
  add_cell(dggs_timeseries) %>%
  filter(measurementType %in% c("Shoot biomass", "Shoot density", "Rhizome biomass", "Cover (%)")) %>%
  group_by(scientificName, date, date_year, cell, measurementType) %>%
  summarize(measurementValue = mean(measurementValue))

#load("shark.rda")
#save(
#  occurrences,
#  occurrence_mof,
#  occurrence_mof_map_biotic,
#  occurrence_mof_timeseries_biotic,
#  file = "shark.rda")  

# dataset

d <- dataset(datasetid = datasetid)
title <- d$title
abstract <- d$abstract

# lists

biotic_species <- sort(unique(occurrence_mof_map_biotic$scientificName))
biotic_types <- sort(unique(occurrence_mof_map_biotic$measurementType))

# map range

xl <- c(min(occurrences$decimalLongitude, na.rm = TRUE), max(occurrences$decimalLongitude, na.rm = TRUE))
yl <- c(min(occurrences$decimalLatitude, na.rm = TRUE), max(occurrences$decimalLatitude, na.rm = TRUE))

# ui

ui <- fluidPage(
  titlePanel(title),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_species", "Species", choices = biotic_species, selected = "Fucus vesiculosus"),
      selectInput("selected_type", "Variable", choices = biotic_types, selected = "Cover (%)"),
      sliderInput("selected_year", "Year", min(occurrences$date_year), max(occurrences$date_year), 2007, step = 1, sep = ""),
      p(abstract)
    ),
    mainPanel(
      leafletOutput("map_biotic", height = 600),
      plotOutput("timeseries_biotic")
    )
  )
)

server <- function(input, output) {

  output$map_biotic <- renderLeaflet({
    
    temp <- occurrence_mof_map_biotic %>%
      filter(scientificName == input$selected_species & measurementType == input$selected_type)
    colorpal <- colorNumeric(palette = "Spectral", domain = temp$measurementValue, reverse = TRUE)

    df <- occurrence_mof_map_biotic %>%
      filter(scientificName == input$selected_species & measurementType == input$selected_type & date_year == input$selected_year) %>%
      add_polygons(dggs_map)
    if (nrow(df) > 0) {
      leaflet(df) %>%
        setView(15, 60, 5) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          color = ~colorpal(as.numeric(measurementValue)),
          fill = ~colorpal(as.numeric(measurementValue)),
          weight = 2,
          fillOpacity = 0.7
        ) %>%
        addLegend(position = "bottomright", colorpal, values = ~as.numeric(measurementValue), bins = 7, title = input$selected_type)
    } else {
      return(NULL)
    }
  })  
  
  output$timeseries_biotic <- renderCachedPlot({
    df <- occurrence_mof_timeseries_biotic %>%
      filter(scientificName == input$selected_species & measurementType == input$selected_type)
    if (nrow(df) > 0) {
      which_cells <- df %>%
        group_by(cell) %>%
        summarize(years = length(unique(date_year))) %>%
        filter(years >= min_years)
      ggplot(data = df %>% filter(cell %in% which_cells$cell), aes(x = date, y = measurementValue, color = as.character(cell))) +
        geom_point() +
        geom_smooth(method = lm, formula = y ~ splines::ns(x, 3), se = FALSE, alpha = 0.1) +
        scale_color_paletteer_d("awtools::a_palette", name = "cell") +
        scale_y_continuous(name = input$selected_type) +
        theme_minimal()
    } else {
      return(NULL)
    }
  }, cacheKeyExpr = { paste0(input$selected_type, input$selected_species) })  
  
}

shinyOptions(cache = memoryCache(max_size = 100e6))
shinyApp(ui = ui, server = server)
