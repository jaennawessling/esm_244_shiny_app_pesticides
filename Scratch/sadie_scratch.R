
library(tidyverse)
library(here)
library(lubridate)
library(dplyr)

### data frames for crop tab ###

###### ANNUAL
crop_annual <- read_csv(here("Tab2_Crop_RiskSummary_Annual.csv"))

#pivot longer
crop_annual_pivot <- crop_annual %>% 
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value")


##### MONTHLY
crop_monthly <- read_csv(here("Tab2_Crop_RiskSummary_Monthly.csv")) %>% 
  separate(col = monthyear, into = c("month", "year"), sep = "-") 

crop_monthly_mod <- crop_monthly %>% 
  mutate(month_num = case_when(month == "Jan" ~ "01",
                               month == "Feb" ~ "02",
                               month == "Mar" ~ "03",
                               month == "Apr" ~ "04",
                               month == "May" ~ "05",
                               month == "Jun" ~ "06",
                               month == "Jul" ~ "07",
                               month == "Aug" ~ "08",
                               month == "Sep" ~ "09",
                               month == "Oct" ~ "10",
                               month == "Nov" ~ "11",
                               month == "Dec" ~ "12")) %>% 
  mutate(date = paste(month_num, year, sep = "-"))  

#lubridate the date column
crop_monthly_mod$date <- my(crop_monthly_mod$date)

# PIVOT LONGER
crop_monthly_pivot <- crop_monthly_mod %>% 
  select(-year, -month, -month_num) %>% 
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value") %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date))


### annual data exploration ###

# make a subset of the data with radio buttons -- select application site type
annual_crop_subset <- crop_annual_pivot %>% 
  #radio button here for selecting application site type
  filter(hru == "Almonds") %>% 
  #select watersheds -> one option for all watersheds
  filter(huc == "All Watersheds") %>%  
  #also select which risk index we want to have
  filter(index_type == "RI_net")
  

# output figure for temporal trends of a single application site
annual_single_crop <- annual_crop_subset %>% 
  ggplot(aes(x = year, y = risk_index_value)) +
  geom_line(color = "orchid", size = 1) +
  labs(x = "Year", y = "Risk Index", title = "Risk Index for Almonds in All Watersheds") +
  theme_minimal()
annual_single_crop  
  

### monthly  data exploration ###

### THIS IS WHAT THE RADIO BUTTONS MAKE HAPPEN
# data subset by month and year
monthly_crop_subset <- crop_monthly_pivot %>% 
  #radio button for application site type
  filter(hru == "Almonds") %>%  #crop_monthly$hru
  #radio button for picking watersheds
  filter(huc == "All Watersheds") %>%  #crop_monthly$huc
  filter(year == 2015) %>% #crop_monthly$year -- want to be able to pick all years?
  filter(index_type == "RI_net")


# output figure for a single application site
monthly_single_crop_fig <- monthly_crop_subset %>% 
  ggplot(aes(x = date, y = risk_index_value, color = year)) +
  geom_line(color = "orchid", size = 1) +
  labs(x = "Date", y = "Net Risk Index", title = "Risk Index for Almonds in All Watersheds in 2015")+
  theme_minimal()
monthly_single_crop_fig
  


# figure with all the animals, without net index type
monthly_animals_fig <- monthly_crop_subset %>% 
  filter(index_type != "RI_net") %>% 
  ggplot(aes(x = date, y  = risk_index_value, fill = index_type)) +
  geom_col() +
  labs(x = "Date", y = "Risk Index", fill = "Risk Index Type") +
  theme_minimal()
monthly_animals_fig



