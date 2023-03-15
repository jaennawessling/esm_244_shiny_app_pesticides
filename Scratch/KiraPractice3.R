library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(broom)
library(leaflet)

### Map Practice ----

### Read in Spatial data with names ----

watershed_annual <- read_csv(here::here("Tab1_Watershed_RiskSummary_Annual.csv"))

watershed_annual_avg <- watershed_annual %>% 
  select(!hru, !pesticide) %>% 
  group_by(year, huc) %>% 
  summarize(avg_net = mean(RI_net), 
            avg_fish = mean(RI_fish), 
            avg_water_invert = mean(RI_invertebrate_water), 
            avg_plant_vasc = mean(RI_plant_vascular), 
            avg_plant_nonvasc = mean(RI_plant_nonvascular),
            avg_sed_invert = mean(RI_invertebrate_sed))
            


watersheds_sf <- read_sf(here::here("spatial_data/BDW_NearHUC12_Watersheds_Simplified/BDW_NearHUC12_Simp10m.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')

rmapshaper::ms_simplify(watersheds_sf)


### Bind spatial data with names/risks ----
watershed_sf_merge <- merge(watersheds_sf, watershed_annual_avg, by.x = "NAME", by.y = "huc") %>%
  st_transform('+proj=longlat +datum=WGS84')

watershed_sf_merge_clean <- watershed_sf_merge %>% 
  janitor::clean_names() %>% 
  select(!huc)

### Create Categories based on percentiles ----
risk_annual_perc <- watershed_sf_merge_clean %>% 
  mutate(quartile = ntile(avg_net, 4)) 

### Color palette for categories ----
fctpal <- colorFactor(palette = c('white', 'yellow', 'orange', 'red'), 
                      levels = c(1, 2, 3, 4))
                      

### Map with Color based on Overall Risk Categories ----
leaflet() %>%
  leaflet::addPolygons(data = risk_annual_perc) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = -121.4194, lat = 37.7749, zoom = 8) %>%
  addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
  addPolygons(data = risk_annual_perc,
              color = ~fctpal(quartile), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0("Watershed: ", risk_annual_perc$name,
                             "<br>",
                             "Risk to Fish ", risk_annual_perc$avg_fish, 
                             "<br>",
                             "Risk to Aquatic Invertebrates: ", risk_annual_perc$avg_water_invert, 
                             "<br>",
                             "Risk to Vascular Plants: ", risk_annual_perc$avg_plant_vasc, 
                             "<br>", 
                             "Risk to Nonvascular Plants: ", risk_annual_perc$avg_plant_nonvasc,
                             "<br>", 
                             "Risk to Terrestrial Invertebrates: ", risk_annual_perc$avg_sed_invert,
                             "<br>",
                             "Net Pesticide Toxicity Risk: ", risk_annual_perc$avg_net))






##### OLD 

# ### Subset excluding zeros ----
# risk_annual_subset_no_zero <- watershed_sf_merge_clean %>% 
#   filter(avg >= 0)
# 
# ### Percentiles without including 0s ----
# risk_subset_0 <- quantile(risk_annual_subset_no_zero$avg, probs = 0)
# risk_subset_25 <- quantile(risk_annual_subset_no_zero$avg, probs = 0.25)
# risk_subset_50 <- quantile(risk_annual_subset_no_zero$avg, probs = 0.5)
# risk_subset_75 <-quantile(risk_annual_subset_no_zero$avg, probs = 0.75)
# 
# risk_annual_perc <- watershed_sf_merge_clean %>%
#   mutate(category = case_when(between(watershed_sf_merge_clean$avg, risk_subset_25, risk_subset_0) ~ "Negligible",
#                               between(watershed_sf_merge_clean$avg, risk_subset_50, risk_subset_25) ~ "Low",
#                               between(watershed_sf_merge_clean$avg, risk_subset_75, risk_subset_50) ~ "Moderate", 
#                               watershed_sf_merge_clean$avg >= risk_subset_75 ~ "High"))
# 


# risk_category <- risk_annual_perc %>% 
#   mutate(category = case_when(watershed_sf_merge_clean$quartile == '1' ~ 'Negligible',
#  watershed_sf_merge_clean$quartile == '2' ~ 'Low',
#  watershed_sf_merge_clean$quartile == '3' ~ 'Moderate',
#  watershed_sf_merge_clean$quartile == '4' ~ 'High'))
