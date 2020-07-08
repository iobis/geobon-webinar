library(robis)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(dggridR)
library(rnaturalearth)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(paletteer)
library(leaflet)
library(shiny)

world <- ne_countries(scale = "medium", returnclass = "sf")

enhance_mof <- function(mof) {
  mof$date <- as.POSIXct(mof$date_mid / 1000, origin = "1970-01-01")
  mof$month <- month(mof$date)
  return(mof)
}

add_cell <- function(df, dggs){
  df$cell <- dgGEO_to_SEQNUM(dggs, df$decimalLongitude, df$decimalLatitude)$seqnum
  return(df)
}

add_polygons <- function(df, dggs) {
  if (nrow(df) > 0) {
    grid <- dgcellstogrid(dggs, frame = FALSE, wrapcells = FALSE, cells = unique(df$cell))
    grid_sf <- st_as_sf(grid)
    grid_sf$cell <- names(grid)
    merged <- merge(grid_sf, df, by.x = "cell", by.y = "cell")
    return(merged)
  } else {
    return(df)
  }
}