source("lib.R")

datasetid <- "f6352e4d-31b5-4baa-99e1-d39646043e88"

d <- dataset(datasetid = datasetid)
title <- d$title

occurrences <- occurrence(datasetid = datasetid, mof = TRUE, event = FALSE)
occurrence_mof <- measurements(occurrences, fields = c("id", "decimalLongitude", "decimalLatitude", "scientificName", "date_year", "date_mid", "species", "genus", "family", "order", "class", "superclass")) %>%
  enhance_mof()

sediment <- occurrence_mof %>%
  filter(measurementType %in% c("fraction of mud", "The mean of grain-size distribution")) %>%
  group_by(decimalLongitude, decimalLatitude, measurementType) %>%
  summarize(
    measurementValue = mean(as.numeric(measurementValue), na.rm = TRUE)
  ) %>%
  pivot_wider(id_cols = c(decimalLongitude, decimalLatitude), names_from = measurementType, values_from = measurementValue) %>%
  rename(mud_fraction = "fraction of mud", grain_size = "The mean of grain-size distribution")

biotic <- occurrence_mof %>%
  filter(measurementType == "abundance" & !is.na(species)) %>%
  group_by(decimalLongitude, decimalLatitude, species) %>%
  summarize(abundance = mean(as.numeric(measurementValue), na.rm = TRUE))

top_species <- biotic %>% group_by(species) %>% summarize(n = n()) %>% arrange(desc(n)) %>% head(20)

ui <- fluidPage(
  titlePanel(title),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_species", "Species", choices = top_species$species, selected = "Spio filicornis"),
      p("Sediment characteristics"),
      plotOutput("plot_sediment"),
      leafletOutput("map_sediment")
    ),
    mainPanel(
      leafletOutput("map_biotic", height = 800)
    )
  )
)

server <- function(input, output) {
  output$plot_sediment <- renderPlot({
    ggplot(sediment) +
      geom_point(aes(x = mud_fraction, y = grain_size))
  })  
  output$map_sediment <- renderLeaflet({
    mud_pal <- colorNumeric(palette = "viridis", domain = sediment$mud_fraction)
    leaflet(sediment) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = ~decimalLongitude, lat = ~decimalLatitude, color = ~mud_pal(mud_fraction), radius = ~grain_size / 50, popup = paste0("<p>mud fraction: <b>", sediment$mud_fraction, "</b><br/>median grain size: <b>", sediment$grain_size, " μm</b>"), weight = 1.5) %>%
      addLegend("bottomright", pal = mud_pal, values = ~mud_fraction, title = "mud fraction")
  })  
  output$map_biotic <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(data = sediment, lng = ~decimalLongitude, lat = ~decimalLatitude, radius = ~grain_size / 50, weight = 0, color = "#339966") %>%
      addCircleMarkers(data = biotic %>% filter(species == input$selected_species), lng = ~decimalLongitude, lat = ~decimalLatitude, radius = ~abundance / 20, weight = 2, popup = paste0("abundance: <b>", biotic$abundance, " (ind/m2)</b>"), fill = NA, color = "#cc3300")
  })  
}

shinyApp(ui = ui, server = server)
