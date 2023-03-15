
library(tidyverse)
library(here)
library(lubridate)
library(dplyr)
library(kableExtra)
library(forcats)

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



### figure of net risk index with top ten most "risky" application site types?
top_crops <- crop_monthly_pivot %>% 
  filter(huc == "All Watersheds") %>% 
  filter(index_type == "RI_net") %>% 
  group_by(hru, date) %>% 
  summarize(mean_ri_net = mean(risk_index_value, na.rm = TRUE))

top_crop_fig <- top_crops %>% 
  group_by(hru, date) %>% 
  slice_max(mean_ri_net, n = 10) %>% 
  ggplot(aes(x = date, y = mean_ri_net, color = hru)) +
  geom_line(size = 1, alpha = 0.8) +
  theme_minimal()
  


top_crop_guess <- crop_monthly_pivot %>% 
  filter(huc == "All Watersheds") %>% 
  filter(index_type == "RI_net") %>% 
  group_by(hru) %>% 
  summarize(mean_ri_net = mean(risk_index_value, rm.na = TRUE)) %>% 
  slice_max(mean_ri_net, n = 10)


#top ten crops for each index, they select an index with a drop down menu
top_crops_trial <- crop_monthly_pivot %>% 
  filter(huc == "All Watersheds") %>%  #this is already done in the top for the datasets
  filter(index_type == "RI_net") %>% # filter(index_type == input$index_type_checkboxes)
  group_by(hru) %>% 
  summarize(mean_ri = mean(risk_index_value, na.rm = TRUE)) %>% 
  slice_max(mean_ri, n = 10) %>% 
  # kable(title = "Top Ten Application Site Types for Each Risk Index") %>% 
  # kable_classic()
  ggplot(aes(x = fct_reorder(hru, mean_ri), y = mean_ri)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
 # scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
  labs(x = "Average risk index across all years", y = "Application site type", title = "10 Application Site Types with Highest Average Risk Index Across All Years")+
  theme_minimal() 






# background info
# fluidRow(
#   column(width=8,
#          
#          h4(strong("The Pesticide Exposure Risk to Plants and Invertebrates Associated with Different Application Site (Crop) Types "), style="text-align:justify;color:black;background-color:lightgreen;padding:15px;border-radius:10px"),
#          p("The figures below show the pesticide exposure risk (risk index) to fish, invertebrates (exposure through water and sediment), vascular plants, and
#                                         nonvascular plants. The overall net risk index can also be displayed. Select with application site type (crop type) to display the risk indices for, and select which risk indices to display.
#                                         Figure 1 shows "
#            
#          ) #end column
#   ), #end fluidRow
#   #dropdown menus
  
  
### Old sidebar panel layout before switching to wellPanel

# sidebarLayout(
#  
#   sidebarPanel(strong("Application Site Type"),
#                #dropdown menu for application site type
#                selectInput("hru_dropdown",
#                            label = "Select an application site type (crop type)",
#                            choices = unique(crop_monthly_final$hru)), #end pesticide dropdown
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                #dropdown menu for watershed 
#                # "Watersheds",
#                # selectInput("watershed_dropdown",
#                #             label = "Select watershed",
#                #             choices = unique(crop_monthly_final$huc)), #end watershed dropdown
#                
#                #dropdown menu for year 
#                strong("Year"),
#                selectInput("year_dropdown",
#                            label = "Select year",
#                            choices = unique(crop_monthly_final$year)), #end year dropdown
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                #checkboxes for risk index 
#                strong("Risk Index Type"),
#                checkboxGroupInput("index_type_checkboxes",
#                            label = "Select risk index type(s)",
#                            choices = unique(crop_monthly_final$index_type),
#                            selected = "RI_net"), #end risk index checkboxes
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                br(),
#                
#                # new index dropdown for top ten crop figures
#                strong("Top Ten Crops with Highest Risk Index"),
#                selectInput("index_top_ten_dropdown",
#                            label = "Pick a risk index type",
#                            choices = unique(crop_annual$index_type)) #end risk dropdown
#                
#                
#                ), #end sidebarPanel 





#color df for line graphs on tab 2 
color_df <- data.frame(variable = c("RI_net", "RI_fish", "RI_invertebrate_water", "RI_invertebrate_sed", "RI_plant_nonvascular", "RI_plant_vascular"), 
                       color = c("red", "orange", "yellow", "green", "blue", "purple"))

#reactive colors in the server for line graph color matching
color_react(reactive{
  color_df %>% 
    filter(variable %in% input$index_type_checkboxes)
})

#in the ggplot
scale_color_manual(breaks = color_react$variable, color = color_react$color)



# QUESTIONS
## can you split up the side bar panel with the widgets into smaller chunks and have them line
# up with the figure that they apply to? -- wellPanel, fixed
## how to make the colors stay the same for each index?  - fixed

## how to get the wellPanel tied to the graph




## trying to figure out how to get the renamed index names onto the dropdowns/graphs

crop_monthly <- read_csv(here("Tab2_Crop_RiskSummary_Monthly.csv"))%>% 
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

#pivot longer -- THIS IS THE FINAL DF TO USE FOR MONTHLY APPLICATION SITE TYPE
crop_monthly_final <- crop_monthly_mod %>% 
  filter(huc == "All Watersheds") %>% 
  select(-year, -month, -month_num) %>% 
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value") %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(hru = str_to_lower(hru)) %>% 
  mutate(index = case_when(index_type == "RI_net" ~ "net risk index",
                           index_type == "RI_fish" ~ "fish",
                           index_type == "RI_invertebrate_water" ~ "aquatic invertebrates",
                           index_type == "RI_invertebrate_sed" ~ "sediment invertebrates",
                           index_type == "RI_plant_vascular" ~ "vascular plants",
                           index_type == "RI_plant_nonvascular" ~ "non-vascular plants"))


crop_annual <- read_csv(here("Tab2_Crop_RiskSummary_Annual.csv")) %>% 
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value") %>% 
  filter(huc == "All Watersheds") %>% 
  mutate(hru = str_to_lower(hru)) %>% 
  mutate(index = case_when(index_type == "RI_net" ~ "net risk index",
                           index_type == "RI_fish" ~ "fish",
                           index_type == "RI_invertebrate_water" ~ "aquatic invertebrates",
                           index_type == "RI_invertebrate_sed" ~ "sediment invertebrates",
                           index_type == "RI_plant_vascular" ~ "vascular plants",
                           index_type == "RI_plant_nonvascular" ~ "non-vascular plants"))



## QUESTIONS/TO DO:
# different margins on the headers for tab 3, how do we get those little gaps?
# need to make the names of the risk indexes consistent -- also clarify the sed inverts -- in the read me it says benthic sediment? so i think we should call them benthic inverts
# weird number issue on the slider bar for the timeline for the graph on the map tab
