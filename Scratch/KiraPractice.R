library(shiny)
library(leaflet)
library(tidyverse)
library(sf)




### Read in Data ----

risk_annual <- read_csv(here::here('Tab1_Watershed_RiskSummary_annual.csv'))


### Subset excluding zeros ----
risk_annual_subset_no_zero <- risk_annual %>% 
  filter(RI_net > 0)

### Percentiles without including 0s ----
risk_subset_0 <- quantile(risk_annual$RI_net, probs = 0)
risk_subset_33 <- quantile(risk_annual$RI_net, probs = 0.33)
risk_subset_66 <- quantile(risk_annual$RI_net, probs = 0.66)

risk_annual_perc <- risk_annual %>%
  mutate(category = case_when(risk_annual$RI_net == 0 ~ "Negligible",
    risk_annual$RI_net >= risk_subset_0 | risk_annual$RI_net <= risk_subset_33 ~ "Low",
    risk_annual$RI_net >  risk_subset_33 | risk_annual$RI_net <= risk_subset_66 ~ "Moderate", 
    risk_annual$RI_net > risk_subset_66 ~ "High"))
    
  

### Read in shapefiles ----
watersheds_sf <- read_sf(here::here("spatial_data/BDW_Watersheds/BDW_Near_HUC12.shp")) %>% 
  st_transform('+proj=longlat +datum=WGS84')


rmapshaper::ms_simplify(watersheds_sf)


### NOTE only geometry column, no linked names

###  Map ---- 

leaflet() %>%
  leaflet::addPolygons(data = watersheds_sf) %>%
addProviderTiles("Esri.WorldTopoMap") %>%
setView(lng = -121.4194, lat = 37.7749, zoom = 8) %>%
addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
addPolygons(data = watersheds_sf,
            color = "Black", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5,
            fillColor = "Pink",
            highlightOptions = highlightOptions(color = "white", weight = 2,
            bringToFront = TRUE), 
            popup = paste0("Watershed: </b>", 
                           "</b>",
                           "Pesticide Risk to Aquatic Ecosystems: </b>",
                           "</b>",
                           "Pesticide Risk to Terrestrial Ecosystems: </b>",
                           "</b>",
                           "Net Pesticide Toxicity Risk: </b>")) 
  # 



# leaflet(watersheds_sf) %>% 
#   addProviderTiles("Esri.WorldTopoMap") %>% 
#   addPolygons(color = "red")




