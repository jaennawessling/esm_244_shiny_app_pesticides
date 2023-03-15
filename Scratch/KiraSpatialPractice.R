library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(broom)
library(leaflet)


### Read in Spatial data with names 

watershed_annual <- read_csv(here::here("Tab1_Watershed_RiskSummary_Annual.csv"))

watershed_annual_avg <- watershed_annual %>% 
  select(huc, year, RI_net) %>% 
  group_by(year, huc) %>% 
  summarize(avg = mean(RI_net))


watersheds_sf <- read_sf(here::here("spatial_data/BDW_NearHUC12_Watersheds_Simplified/BDW_NearHUC12_Simp10m.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')

rmapshaper::ms_simplify(watersheds_sf)

## Bind spatial data with names/risks
watershed_sf_merge <- merge(watersheds_sf, watershed_annual_avg, by.x = "NAME", by.y = "huc") %>%
  st_transform('+proj=longlat +datum=WGS84')

watershed_sf_merge_clean <- watershed_sf_merge %>% 
  janitor::clean_names() %>% 
  select(!huc)

### Reactive df for watershed 
# watershed_by_yr <- watershed_annual %>% 
#     # filter(year %in% (input$tox_yr_slider[1]:input$tox_yr_slider[2])) %>% 
#     # filter(huc %in% c(input$watershed_select)) %>% 
#     group_by(year, huc) %>% 
#     summarize(totals = sum(RI_net))
  


unique_watersheds <- watershed_annual %>%
  select(huc, RI_invertebrate_water, RI_invertebrate_sed, RI_net) %>%
  distinct()

# write_csv(unique_watersheds, "watershed_data_processed.csv")

leaflet() %>%
  leaflet::addPolygons(data = watershed_sf_merge_clean) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = -121.4194, lat = 37.7749, zoom = 8) %>%
  addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
  addPolygons(data = watershed_sf_merge_clean,
              color = "Black", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "Pink",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))


# ,
#               popup = paste0("Watershed: </b>", unique_watersheds$huc, "</b>",
#                              "Pesticide Risk to Aquatic Ecosystems: </b>", unique_watersheds$RI_invertebrate_water,
#                              "</b>",
#                              "Pesticide Risk to Terrestrial Ecosystems: </b>", unique_watersheds$RI_invertebrate_sed,
#                              "</b>",
#                              "Net Pesticide Toxicity Risk: </b>", unique_watersheds$RI_net))
#               # 
#               # 
