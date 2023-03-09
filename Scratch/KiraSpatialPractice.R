library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(broom)
library(leaflet)


### Read in Spatial data with names 

watershed_annual <- read_csv(here("Tab1_Watershed_RiskSummary_Annual.csv"))


watersheds_sf <- read_sf(here::here("spatial_data/BDW_NearHUC12_Watersheds_Simplified/BDW_NearHUC12_Simp10m.shp")) %>% 
  st_transform('+proj=longlat +datum=WGS84')

rmapshaper::ms_simplify(watersheds_sf)


### Reactive df for watershed 
watershed_by_yr <- watershed_annual %>% 
    # filter(year %in% (input$tox_yr_slider[1]:input$tox_yr_slider[2])) %>% 
    # filter(huc %in% c(input$watershed_select)) %>% 
    group_by(year, huc) %>% 
    summarize(totals = sum(RI_net))
  
### Bind spatial data with names/risks
watershed_sf_merge <- merge(watersheds_sf, watershed_annual, by.x = "NAME", by.y = "huc") %>%
  st_transform('+proj=longlat +datum=WGS84')

# leaflet() %>%
#   leaflet::addPolygons(data = watersheds_sf_merge) %>%
#   addProviderTiles("Esri.WorldTopoMap") %>%
#   setView(lng = -121.4194, lat = 37.7749, zoom = 8) %>%
#   addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
#   addPolygons(data = watersheds_sf_merge,
#               color = "Black", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               fillColor = "Pink",
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE), 
#               popup = paste0("Watershed: </b>", 
#                              "</b>",
#                              "Pesticide Risk to Aquatic Ecosystems: </b>",
#                              "</b>",
#                              "Pesticide Risk to Terrestrial Ecosystems: </b>",
#                              "</b>",
#                              "Net Pesticide Toxicity Risk: </b>"))
# 
# 
