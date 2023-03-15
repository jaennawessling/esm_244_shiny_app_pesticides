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
            avg_sed_invert = mean(RI_invertebrate_sed)) %>%
  mutate(net_quart = ntile(avg_net, 4),
         fish_quart = ntile(avg_fish, 4),
         water_invert_quart = ntile(avg_water_invert, 4),
         plant_v_quart = ntile(avg_plant_vasc, 4),
         plant_nv_quart = ntile(avg_plant_nonvasc, 4),
         sed_quart = ntile(avg_sed_invert, 4)) %>%
  select(year, huc, net_quart, fish_quart,
         water_invert_quart, plant_v_quart,
         plant_nv_quart, sed_quart) %>% 
  mutate(net_quart = case_when(net_quart == "1" ~ "negligible",
                               net_quart == "2" ~ "low",
                               net_quart == "3" ~ "moderate",
                               net_quart == "4" ~ "high"),
         fish_quart = case_when(fish_quart == "1" ~ "negligible",
                                fish_quart == "2" ~ "low",
                                fish_quart == "3" ~ "moderate",
                                fish_quart == "4" ~ "high"),
         water_invert_quart = case_when(water_invert_quart == "1" ~ "negligible",
                                        water_invert_quart == "2" ~ "low",
                                        water_invert_quart == "3" ~ "moderate",
                                        water_invert_quart == "4" ~ "high"),
         plant_v_quart = case_when(plant_v_quart == "1" ~ "negligible",
                                   plant_v_quart == "2" ~ "low",
                                   plant_v_quart == "3" ~ "moderate",
                                   plant_v_quart == "4" ~ "high"),
         plant_nv_quart = case_when(plant_nv_quart == "1" ~ "negligible",
                                    plant_nv_quart == "2" ~ "low",
                                    plant_nv_quart == "3" ~ "moderate",
                                    plant_nv_quart == "4" ~ "high"),
         sed_quart = case_when(sed_quart == "1" ~ "negligible",
                               sed_quart == "2" ~ "low",
                               sed_quart == "3" ~ "moderate",
                               sed_quart == "4" ~ "high")) %>% 
  pivot_longer(net_quart:sed_quart, names_to = "index_type", values_to = "quartile")


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
# this will go into reactive portion 
risk_annual_perc <- watershed_sf_merge_clean %>% 
  filter(year %in% '2016')   

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
              color = ~fctpal(net_quart), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0("Watershed: ", risk_annual_perc$name,
                             "<br>",
                             "Risk to Fish ", risk_annual_perc$fish_quart, 
                             "<br>",
                             "Risk to Aquatic Invertebrates: ", risk_annual_perc$water_invert_quart, 
                             "<br>",
                             "Risk to Vascular Plants: ", risk_annual_perc$plant_v_quart, 
                             "<br>", 
                             "Risk to Nonvascular Plants: ", risk_annual_perc$plant_nv_quart,
                             "<br>", 
                             "Risk to Terrestrial Invertebrates: ", risk_annual_perc$sed_quart,
                             "<br>",
                             "Net Pesticide Toxicity Risk: ", risk_annual_perc$net_quart))






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

# watershed_annual_categories <- watershed_annual_avg %>% 
#   ifelse(watershed_annual_avg$net_quart:sed_quart == 1, "Negligible", 
#           watershed_annual_avg$net_quart:sed_quart == 2, "Low", 
#           watershed_annual_avg$net_quart:sed_quart == 3, "Moderate", 
#           watershed_annual_avg$net_quart:sed_quart == 4, "High")
# replace(c(1, 2, 3, 4), c("Negligible", "Low", "Moderate", "High"))

map_color_df <- data.frame(quartile = c("negligible", "low", "moderate", "high"),
                           color = c('#d0c1db', '#DBA507', '#CC7351', '#540B0C'))
