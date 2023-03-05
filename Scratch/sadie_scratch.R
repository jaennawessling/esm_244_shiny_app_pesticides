
library(tidyverse)
library(here)

# data frames for crop tab
crop_annual <- read_csv(here("Tab2_Crop_RiskSummary_Annual.csv"))

crop_monthly <- read_csv(here("Tab2_Crop_RiskSummary_Monthly.csv"))


# annual data exploration

# make a subset of the data with radio buttons -- select application site type
annual_subset <- crop_annual %>% 
  #radio button here for selecting application site type
  filter(hru == "Almonds") %>% 
  #select watersheds?
  filter(huc == "All Watersheds")

# output figure for temporal trends of a single application site
annual_single_crop <- annual_subset %>% 
  ggplot(aes(x = year, y = RI_net)) +
  geom_line(color = "orchid") +
  labs(x = "Year", y = "Net Risk Index", title = "Risk Index for Almonds in All Watersheds")+
  theme_minimal()
annual_single_crop  
  


