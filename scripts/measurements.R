library(robis)
library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(rnaturalearth)
library(paletteer)
library(sf)
options(robis_use_cache = TRUE)
coastline <- rnaturalearth::ne_coastline(returnclass = "sf", scale = "large")

# fetch polychaete occurrences for the southern North Sea

occ <- occurrence("Polychaeta", geometry = "POLYGON ((1.09863 52.71633, 1.04370 51.05521, 2.43896 50.59021, 4.81201 52.25471, 1.09863 52.71633))", mof = TRUE)

# extract measurements and keep only abundance

mof <- measurements(occ, fields = c("dataset_id", "decimalLongitude", "decimalLatitude", "date_year", "date_mid", "species", "catalogNumber")) %>%
  filter(!is.na(species) & str_detect(measurementType, regex("abundance", ignore_case = TRUE)))

table(mof$dataset_id)
table(mof$measurementType, mof$measurementUnit)

# filter abundance measurements in units #/m2

mof <- mof %>%
  filter(measurementUnit %in% c("#/m^2", "#/m2", "#/m²"))

table(mof$dataset_id)
table(mof$measurementType, mof$dataset_id)

# plot by dataset

ggplot() +
  geom_sf(data = coastline) +
  geom_point(data = mof, aes(x = decimalLongitude, y = decimalLatitude, color = dataset_id, shape = dataset_id), size = 2) +
  paletteer::scale_color_paletteer_d("beyonce::X66") +
  scale_shape_manual(values = seq(1:10)) +
  theme_void() +
  coord_sf(xlim = range(mof$decimalLongitude), ylim = range(mof$decimalLatitude))

# find most represented species

mof %>%
  group_by(species) %>%
  summarize(n_datasets = n_distinct(dataset_id)) %>%
  arrange(desc(n_datasets))

# visualize Nephtys hombergii abundance

n_hom <- mof %>%
  filter(species == "Nephtys hombergii")

colorpal <- colorNumeric(palette = "Spectral", domain = as.numeric(n_hom$measurementValue), reverse = TRUE)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = n_hom, lng = ~decimalLongitude, lat = ~decimalLatitude,
                   radius = ~as.numeric(measurementValue) / max(as.numeric(measurementValue)) * 30,
                   color = ~colorpal(as.numeric(measurementValue)),
                   weight = 2,
                   opacity = 1,
                   fillOpacity = 0,
                   popup = paste0("abundance: <b>", n_hom$measurementValue, " ind/m2</b><br/>catalogNumber: <b>", n_hom$catalogNumber, "</b>"))
