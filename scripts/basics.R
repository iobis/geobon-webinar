library(robis)
library(dplyr)
library(ggplot2)
library(leaflet)
library(stringr)
library(tidyr)
library(paletteer)
options(robis_use_cache = TRUE)

# fetching data

df <- occurrence("Abra alba")
df

# creating checklists

df <- checklist("Semelidae")
df

ggplot(head(df, n = 20)) +
  geom_bar(aes(scientificName, records, fill = taxonRank), stat = "identity") +
  scale_fill_paletteer_d("calecopal::bigsur") +
  scale_x_discrete(limits = rev(head(df, n = 20)$scientificName)) +
  coord_flip() +
  theme_minimal()

# filters

g <- get_geometry()
g
df <- occurrence("Abra alba", startdate = "2010-01-01", geometry = g)
df

# simple maps

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = df, ~decimalLongitude, ~decimalLatitude, radius = 3, color = "tomato", weight = 0, fillOpacity = 0.5)

# quality flags

table(df$flags)

ggplot() +
  geom_point(df, mapping = aes(bathymetry, depth, color = str_detect(replace_na(flags, ""), "depth_exceeds_bath")), shape = 1) +
  scale_color_manual(name = "depth_exceeds_bath", values = c("palegreen3", "tomato")) +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
  scale_y_reverse()

df_nok <- occurrence("Abra alba", flags = "on_land")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = df, ~decimalLongitude, ~decimalLatitude, radius = 2, weight = 0, color = "darkseagreen") %>%
  addCircleMarkers(data = df_nok, ~decimalLongitude, ~decimalLatitude, radius = 4, weight = 1, color = "tomato")

# measurements or facts

df <- occurrence("Abra alba", mof = TRUE)
mof <- measurements(df)

View(sort(table(mof$measurementType), TRUE))


