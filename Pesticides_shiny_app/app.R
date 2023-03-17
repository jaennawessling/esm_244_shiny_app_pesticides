#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(sf)
library(leaflet)
library(lubridate)
library(forcats)
library(plotly)
library(bslib) 
library(shinythemes)
library(dplyr)
library(shinycssloaders)


#######################################################################################
### Model Output Data
#######################################################################################

#### Tab 1 annual data: annual watershed risk summary ----
watershed_annual <- read_csv(here("Tab1_Watershed_RiskSummary_Annual.csv"))


### Tab 1 spatial watersheds
watersheds_sf <- read_sf(here::here("spatial_data/BDW_NearHUC12_Watersheds_Simplified/BDW_NearHUC12_Simp10m.shp")) %>% 
  st_transform('+proj=longlat +datum=WGS84')

rmapshaper::ms_simplify(watersheds_sf)

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
  rename("fish" = fish_quart, "aquatic invertebrates" = water_invert_quart, "benthic invertebrates" = sed_quart,
         "vascular plants" = plant_v_quart, "non-vascular plants" = plant_nv_quart, "net risk" = net_quart) %>% 
  pivot_longer("net risk":"benthic invertebrates", names_to = "index_type", values_to = "quartile")



### Tab 1 Bind spatial data with names/risks 
watershed_sf_merge <- merge(watersheds_sf, watershed_annual_avg, by.x = "NAME", by.y = "huc") %>%
  st_transform('+proj=longlat +datum=WGS84')

watershed_sf_merge_clean <- watershed_sf_merge %>% 
  janitor::clean_names() %>% 
  select(!huc)


#######################################################################################
#### Tab 2 annual data: annual crop risk summary
crop_annual <- read_csv(here("Tab2_Crop_RiskSummary_Annual.csv")) %>% 
  rename("fish" = RI_fish, "aquatic invertebrates" = RI_invertebrate_water, "benthic invertebrates" = RI_invertebrate_sed,
        "vascular plants" = RI_plant_vascular, "non-vascular plants" = RI_plant_nonvascular, "net risk" = RI_net) %>% 
  pivot_longer("fish":"net risk", names_to = "index_type", values_to = "risk_index_value") %>% 
  filter(huc == "All Watersheds") %>% 
  mutate(hru = str_to_lower(hru)) 
 

#### Tab 2 monthly data: monthly crop risk summary
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
  rename("fish" = RI_fish, "aquatic invertebrates" = RI_invertebrate_water, "benthic invertebrates" = RI_invertebrate_sed,
         "vascular plants" = RI_plant_vascular, "non-vascular plants" = RI_plant_nonvascular, "net risk" = RI_net) %>% 
  pivot_longer("fish":"net risk", names_to = "index_type", values_to = "risk_index_value") %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(hru = str_to_lower(hru))
  


#######################################################################################
#### Tab 3 data: days exceeding health benchmarks

## Loading in data and renaming the column names
days_exceed <- read_csv(here("Tab3_Days_ExceedHealthBenchmarks.csv")) %>% 
  rename(fish = days_fish) %>% 
  rename("aquatic invertebrates" = days_invertebrate_water) %>% 
  rename("benthic invertebrates" = days_invertebrate_sed) %>% 
  rename("non-vascular plants" = days_plant_nonvascular) %>% 
  rename("vascular plants" = days_plant_vascular) %>% 
  rename("any taxa" = days_any_species)  

## Making a longer data frame to work with 
exceed_longer <- days_exceed %>% 
  pivot_longer(cols = fish:"any taxa", names_to = "species", values_to = "days") %>% 
  rename(watersheds = huc) %>% 
  mutate(pesticide = str_to_lower(pesticide),
         crop = str_to_lower(crop)) 

##  Filtering only the days of exceedance for each individual species - but not "any" species)
watershed_species_risk <- exceed_longer %>% 
  select(species, pesticide, watersheds, days) %>% 
  filter(species != "any taxa")


#######################################################################################
### Spatial Data
#######################################################################################

# watershed outline shapefile
watershed_shp <- read_sf(here("spatial_data", "BDW_Watersheds", "BDW_Near_HUC12.shp"))


#######################################################################################
### Color Palette
#######################################################################################
# main color: #85d6a5

#### Tab 1 map data frame
# high, low, moderate, negligible
map_colors <- colorFactor(c('#320607', '#a98585', '#763b3c', '#ede6e6'), watershed_annual_avg$quartile)

#original map colors if we want to switch back
#c('#540B0C', '#ba9d9d', '#875454', '#ede6e6')


#### Tab 2 reactive color data frame
color_df <- data.frame(variable = c("net risk", "fish", "aquatic invertebrates", "benthic invertebrates", "non-vascular plants", "vascular plants"), 
                       color = c("#85d6a5", "#00796b", "#DBA507", "#CC7351", "#8EC7D2", "#d0c1db"))

### Creating a vector version of this, not connected to specific variables
our_colors = c("#85d6a5", "#00796f", "#DBA507", "#CC7354", "#8EC7D2", "#d0c1db", "#355C7F", "#A23E49",
                        "#4d3591", "#966E5C", "#9B945F", "#ADDFB3", "#F2ACB9", "#A8A9AD", "#483C32",
                        "#BBECF2", "#540B0C")
                        


#######################################################################################
## Theme
#######################################################################################
my_theme <- bs_theme(
  bootswatch = "minty")



#######################################################################################
### Define UI ---- 
#######################################################################################

ui <- fluidPage(theme = my_theme, 

                # Application title
                titlePanel("Pesticide Risk in the San Francisco Bay Delta Watershed"),
                
                # Adding our tabs panel
                tabsetPanel(

                  # Welcome Tab - Jaenna ----
                  tabPanel(icon("home"),
                           
                           # Adding an image to the front page
                           imageOutput("crissy_field"),
                           
                           # Adding text beneath photo for credits
                           p(em("Photo of Crissy Field, San Francisco. (Photo by Will Elder, 
                              courtesy of the National Park Service)"), style="text-align: center; font-size:12px"
                           ), # end photo text
                           hr(), # horizontal line break
                           
                           
                           # Creating a fluid row to can create multiple columns to have information and a small photo
                           fluidRow(
                             column(width=8,
                                    h4(strong("Purpose"), style="text-align:justify;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                                    p("This interactive tool illustrates pesticide risk based on toxicity to fish, aquatic invertebrates, aquatic nonvascular plants (algae), 
                                    and aquatic vascular plants in the (San Francisco) Bay Delta Watershed."), # End paragraph 1 
                                    br(), # Line break
                                    
                                    h4(strong("Background"), style="text-align:justify;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                                    strong("What does the Pesticide Management Prioritization Model (PMPM) - Environmental Fate Tool do?"),
                                    p("The data used in this analysis originated from the Environmental Fate Tool which analyzes pesticide risks across the United States. 
                     The Environmental Fate Tool (EFT) is the second model of the Pesticide Management Prioritization Model (PMPM), 
                     which predicts spatiotemporal explicit concentrations of pesticides from agricultural use in soil, water, and sediment. The use
                     data is compiled from pesticide use reports with data at the daily time-step (required
                     by growers in CA). Pesticide concentrations are predicted using mechanistic models that
                     consider climate, hydrology, irrigation practices, and pesticide properties in the 
                     watersheds within ~100 km of the Bay Delta Watershed (22,000 square km)."),
                     br(),
                     p("For the analysis in this website, pesticide exposure risk and days of exceedance of 
                       pesticide concentration are utilized. 
                       The risk index is a measurement of the net toxic load of pesticides in soil and surface water, which indicates
                       how much total toxicity is in the environment over time. 
                       The days of exceedance are the number of days that concentrations of pesticides exceed concentrations lethal to aquatic organisms.
                       This shows how many runoff events in the simulation period exceeded these lethal concentrations."),
                     
                     br(),
                     
                     p(strong("The PMPM and EFT are designed to address limitations of existing tools for pesticide impact
                    analyses by integrating features into a single tool that can:")),
                    p("1) Quantify the risk to diverse taxa over tens of thousands of kilometers (hundreds of watersheds)."),
                    p("2) Evaluate primary sources of pesticide risk as well as their temporal variability."),
                    p("3) Analyze the cumulative risk of the hundreds of pesticides in use."),
                    p("4) Quantify how often pesticide concentrations are predicted to exceed."),
                 
                    br(),
                    strong("Why is this analysis needed?"),
                    p("13% of California’s 
                     waterways are designated as impaired by pesticides of those assessed for non-point 
                     source pollution under the Clean Water Act. 56% are present within the Bay Delta 
                     Watershed (BDW), home to over 90 threatened and endangered species."),
                    
                    br(),
                    strong("What are the PMPM and the Environmental Fate Tool goals?"),
                    p("As humans move toward pesticides that are lower in toxicity for mammals, but are orders of magnitude
                     more toxic to invertebrates and aquatic organisms, the PMPM and EFT aim to identify:"),
                    p("1) Which activities are imposing the greatest pesticide loads?"),
                    p("2) Who is responsible?"),
                    p("3) How can tradeoffs between the benefits of chemical use be managed to restore
                     and preserve ecosystem health?") # end of background section 
                             ), # end column 5 fluidrow 
                    
                    column(
                      br(),
                      tags$img(src="watershed.jpg",width="250px",height="310px", align = "justify"),
                      br(),
                      br(), 
                      p("The Bay Delta Watershed. The various colored regions represent the five main areas of the watershed.
                        In this analysis, only portions of the San Francisco Bay and the Sacramento-San 
                        Joaquin River Delta regions (dark green and orange regions) were analyzed. 
                        (Photo courtesy of the United States Environmental Protection Agency.)",
                        br(),
                        style="text-align:justify;color:black, font-size:12px"),
                      width=3) ## End first fluid row column
                           ), # end fluidrow 1
                    
                    fluidRow(
                      column(width=8,
                             
                             ## Website contents
                             h4(strong("Website Content"), style="text-align:justify;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                             p("This website is comprised of three main tabs:"), 
                             br(),
                             
                             p(strong("Tab 1: Map of Pesticide Risk")), 
                             p("An interactive map of the pesticide exposure risk to plant and animal taxa by watersheds within the San Francisco Bay Delta
                               watershed. The map colors indicate risk severity, with each risk index divided into percentile categories based on their yearly averages:
                               Negligible Risk, Low Risk, Moderate Risk, and High Risk. Additionally, there is an interactive graph depicting total risk based on 
                               annual toxicity data."), 
                       br(),
                       
                       p(strong("Tab 2: Temporal Trends by Application Site Type")), 
                       p("Interactive time series graphs of the pesticide exposure risk to fish, invertebrates 
                       (exposure through water or sediment), vascular plants, and nonvascular plants. The graphs are grouped
                       by application site type (crop type). Application site types describe the different types of crops associated with pesticide
                       use in the Bay Delta Watershed. The overall net risk index can also be displayed."), 
                       br(),
                       
                       p(strong("Tab 3: Pesticide Exceedance on Taxa and Crops")), 
                       p("Interactive bar charts of the modeled number of days a pesticide in water 
                    exceeded the concentration at which severe and adverse effects would occur for various 
                    crops, and aquatic and sediment species. For the purpose of this analysis, only the top 
                    15 counts of days of exceedance were selected for each bar chart. The bar charts are grouped by watershed."), 
                    br(),
                      ), # End column 7
                    
                    column(width=3,
                           br(),
                           tags$img(src="crops.png",width="400px",height="460px", align = "justify"),
                           br(),
                           br(), 
                           p("Plant nursery, Almond orchard, and Christmas tree farm. Various application site types within the Bay Delta Watershed.
                             (Photos courtesy of Modern Farmer, The Almond Doctor, and California Crossroads.)",
                           br(),
                           style="text-align:justify;color:black, font-size:12px"),
                    ) ## End column 6 - fluidrow
                    ), # end fluid row
                    
                    
                    fluidRow(
                      
                      ## Data information 
                      column(width=8,
                             h4(strong("Data Summary"), style="text-align:justify;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                             p("In this analysis, 226 watersheds within the Bay Delta Watershed were analyzed. 
                               Only portions of the San Francisco Bay and the Sacramento-San 
                               Joaquin River Delta regions were analyzed."),
                             br(),
                             p("Only pesticides contributing to the top 99.5% of toxicity levels were included in this analysis, 
                               the remaining 0.05% were omitted due to data size limitations and overall clarity."),
                             br(),
                             p("There were five taxa analyzed in this data set including fish, aquatic and benthic invertebrates, and vascular and non-vascular plants. 
                               There were 39 crop (application site) types including almonds, grapes, flowers, nurseries, Christmas tree farms, fallow, olives, wheat, and more. 
                               The model data years ranged from 2015 - 2019."),
                     
                      br(),
             
                      ## Data sourcing 
                     
                             h4(strong("Data Source"), style="text-align:justify;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                             p("Data sourced from Nicol Parker, PhD Candidate at the University of California, 
                      Santa Barbara, Bren School of Environmental Science & Management. With support from the 
                      Bay Delta Science Fellowship, and initiative of the California Sea Grant."),
                      
                      br(),
                      
                      tags$p(HTML("To download the data and userguide, click 
                                <a href=\"https://datadryad.org/stash/share/7a-F-jEXmlvWi3-xeRx_X4osZqXrr8Nh97tnx2bBOSk/\">here.</a>"))
                     
                     ), ## End data source column
             
                      ## Species photo column
                      column(width=3,
                             br(),
                             tags$img(src="species.png",width="400px",height="460px", align = "justify"),
                             br(),
                             br(), 
                             p("Polychaete, prickly sculpin, and rock crab. Aquatic and benthic fish and invertebrate species of the Bay Delta Watershed.
                               (Photos courtesy of iNaturalist Canada, Pearson Ecological, and Walla Walla University.)",
                             br(),
                             style="text-align:justify;color:black, font-size:12px"),
                      ), ## End column 4 - species photo column
                     hr()
                     ), # end fluidrow 3
                    
                   
                    ## Adding our development credits
                    p(em("Developed by"),br("Kira Archipov, Sadie Cwikiel, and Jaenna Wessling"),style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                    br(),
                    br(),
                    
                   
                  
                  ), # End tabPanel - Welcome Page
                  
                  #######################################################################################
                  # Tab 1 - Map tab - Kira ----
       
                  tabPanel("Map of Pesticide Risk", 
                           hr(),
                           
                           h5("Risk of Pesticide Exposure in the Bay Delta Watershed", style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                         
                           ###### Map and map widgets
                           fluidRow(
                             
                             br(),
                             
                             br(),
                             
                            
                             p("Below is an interactive map of watersheds within the Bay Delta region, color indicates risk severity. 
                               Each risk index has been divided into percentile categories based on their yearly averages:
                               Negligible Risk, Low Risk, Moderate Risk, and High Risk. Years range from 2015 to 2019, and risk indices include overall risk, risk to fish, 
                              risk to vascular and nonvascular plants, and risk to aquatic and benthic invertebrates."),
                             
                             br(), 
                             p(strong("The purpose of this tab is to:")), 
                             p("Visually categorize the risk to diverse taxa over several years and tens of thousands of kilometers (hundreds of watersheds)."),
                             hr(),
                             br(),
                             
                             p(strong("Select a year and risk index to begin.")),
                             ### widgets for map
                             column(3, position = "right",
                                           
                                      #select year
                                      wellPanel(
                                        selectInput('year_map', 
                                                    label = 'Select year:', 
                                                    choices = unique(watershed_annual_avg$year),
                                                    "2015", 
                                                    multiple = FALSE),
                                        
                                      ), #end year wellPanel
                                    
                                    br(),
                                           
                                      #select index
                                      wellPanel(
                                        selectInput('index_map', 
                                                    label = 'Select risk index type:', 

                                                    choices = unique(watershed_annual_avg$index_type)) 
                                            
                                      ), #end index wellPanel
                                   
                                            
                                    ), # end map widget column
                       
                             
                             ### MAP
                             mainPanel(
                               column(12,
                                      
                                      # Map Title 
                                      tags$strong("Pesticide Risk in Watersheds Surrounding the Bay Delta"), 
                                      
                                      #Leaflet map - 
                                      leafletOutput("risk_map") %>% 
                                        withSpinner(color = "#00796b", type = 4, size = 1)
                                      
                                      
                                      
                                      ) #end column
                               
                             ) #end mainPanel
                        
                           ), #end fluidRow -- MAP
                           
                           br(),
                           
                           br(),
                           
                           br(),
                        
                          
                           ##### fluidRow for graph and graph widgets
                           fluidRow(
                             
                             p("The graph below depicts total risk based on annual toxicity data. 
                               Use the above map to determine which watersheds you wish to focus on."),
                             p(strong("Then select watershed names and year ranges below to see how net risk has changed through time. 
                               Multiple watersheds and years may be selected.")),
                             
                             column(3,
                                    #widgets for graph 
                                    wellPanel(
                                              
                                              selectInput('watershed_select', 
                                                          label = 'Select watershed(s):', 
                                                          choices = unique(watershed_annual$huc),
                                                          "Antelope Creek", 
                                                          multiple = TRUE), #END Watershed dropdown
                                              
                                              sliderInput("tox_yr_slider", 
                                                          label = "Select year range:", 
                                                          min = 2015, 
                                                          max = 2019, value = c(2016, 2018), 
                                                          sep = "") #END year slider 
                                              
                                    ), # END wellPanel - Map tab
                                    
                              ), #end column
                             
                           
                             ### Graph mainPanel
                             mainPanel(
                               column(12,
                                      tags$strong("Overall Pesticide Toxicity Risk by Watershed Over Time"),
                                      
                                      plotlyOutput(outputId = 'watershed_yr_plot') %>% 
                                        withSpinner(color = "#00796b", type = 4, size = 1)
                                 
                               ), #end graph column
                               br(), 
                               br(),
                               
                             ) #end graph mainPanel
                     
                           ) #end fluidRow -- graph
                           
                  ), # END tabPanel - map
                  
                 
                  
                  #######################################################################################
                  # Tab 2 - Application site type (crop) data - Sadie ----
                  
                  
                  tabPanel("Temporal Trends by Application Site Type", 
                           hr(),
                           
                           h5("The Pesticide Exposure Risk Index for Plants and Invertebrates for Different Application Site Types ", style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                           
                           fluidRow(
                        
                             p("Application site types describe the different croplands associated with pesticide use in the Bay Delta Watershed. Different amounts and types of pesticides are applied to each variety of crop at different times of the year. 
                             The figures below show the pesticide exposure risk (risk index) to fish, invertebrates (in water or benthic sediment), vascular plants, nonvascular plants, and the overall net risk index. 
                             The net risk index is the risk index observed for all taxa evaluated, summarized across all taxa To change the information displayed on each chart, select different application site types, years, and risk indexes."),
                             
                             p(strong("The purpose of this tab is to:")), 
                             p("1) Evaluate primary sources of pesticide risk as well as their temporal variability."),
                             p("2) Analyze the cumulative risk of the hundreds of pesticides in use."),
                             p("3) Understand which activities are imposing the greatest pesticide risks to fish, invertebrates, and plants."),
                             p("4) Compare pesticide exposure risk between taxa."),
                             

                             hr(),
                             br(),
                             
                             column(3,
                                    
                                    br(), 
                                    
                                    # drop down menu for application site type 
                                    wellPanel(
                                     # strong("Select an Application Site Type (crop type):"),
                                          selectInput("hru_dropdown",
                                                      label = "Select an application site type (crop type):",
                                                      choices = unique(crop_monthly_final$hru)) #end pesticide dropdown
                                    ), # end wellPanel
                                    
                                    br(),
                                    
                                    #dropdown menu to select year
                                    wellPanel( 
                                      #strong("Select a Year:"),
                                      selectInput("year_dropdown",
                                                  label = "Select a year:",
                                                  choices = unique(crop_monthly_final$year)) #end year dropdown
                                
                                    ), # end wellPanel
                                    
                                    br(),
                                    
                                    #checkboxes for risk index 
                                    wellPanel(
                                      #strong("Select Risk Index Type(s):"),
                                      checkboxGroupInput("index_type_checkboxes",
                                                         label = "Select risk index type(s):",
                                                         choices = unique(crop_monthly_final$index_type),
                                                         selected = "net risk") #end risk index checkboxes
                                    ) #end wellPanel
                              
                                ), #end column
                           
                           
                             #display  the graphs of temporal trends for the selected application site type
                             mainPanel(
                               column(12, 
                                      
                                      br(),
                                      
                                      p("The figure below illustrates the selected risk indexes for a selected application site type in one given year. Select which application site type (crop type), risk indexes, and year to display."),
                                      
                                      # Figure 1
                                       #strong("Figure 1: Temporal Trends by Application Site Type in Selected Year"),
                                      plotlyOutput(outputId = 'hru_monthly_plot'), #tell the app where to put the graph
                                       
                                      br(), 
                                       
                                      br(),
                                      
                                      br(),
                                       
                                      # Figure 2
                                      
                                      p("The following figure shows the selected risk indexes for a selected application site type across all years of the analysis, 2015-2019. Select which application site type (crop type) and risk indexes to display across all years of the data."),
                                      
                                      plotlyOutput(outputId = 'hru_annual_plot'),
                                       
                                      br(),
                                       
                                      br(),
                                       
                                      br(),
                                       
                                      br(),
                                     
                               ) #end column        
                             ) #end mainPanel
                           ), #end fluidRow 
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    # risk index dropdown for top ten crop figures (does not impact line graphs)
                                    wellPanel(
                                     # strong("Risk Index Type"),
                                      selectInput("index_top_ten_dropdown",
                                                  label = "Select a risk index type:",
                                                  choices = unique(crop_annual$index_type)) #end risk dropdown
                                    ) # end wellPanel
                              ), #end column
                             
                             mainPanel(
                                    column(12,
                                           
                                           #Figure 3: top ten crops that contribute to risk for each index
                                           p("The figure below shows which ten application site types contribute the most pesticide exposure risk for each index type.
                                             Select which risk index type to display."),
                                       
                                           plotlyOutput(outputId = 'top_ten_crops')  
                                          
                                           
                                    ), # end column
                                    br(), 
                                    br(),
                             ) #end mainPanel
                            
                           ) # end fluidRow
                  ), #end tabPanel - temporal trends by application site type


                  #######################################################################################
                  # Tab 3 - Species tab - Jaenna ----
                  # Species tab - Jaenna ----
                  tabPanel("Days of Pesticide Exceedance",
                           hr(),
                           h5("Days of Pesticide Exceedance by Taxa and Crop", style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                           p("The figures below illustrate the modeled number of days in the simulation period that concentrations of pesticides
                             exceeded the concentration at which they are lethal to 
                               aquatic taxa, which include vascular and nonvascular plants, fish, and aquatic and benthic invertebrates."),
                           p("For this analysis, 
                             only the top 15 counts of days of exceedance were selected for 
                             each bar chart."),
                    
                       
                           br(),
                           p(strong("The purpose of this tab is to explore:")), 
                           p("1) How often pesticide concentrations are modeled to exceed the concentration at which severe and adverse effects occur."),
                           p("2) How pesticide concentration exceedance differs between animals, plants, and crops."),
                           p("3) How the days of exceedance in animals, plants, and crops differ in each watershed."),
                           p("4) Which pesticide types most often contribute to days of exceedance in animals, plants, and crops."),
                           
                           
                           br(), 
                           p("Comparing these variables may increase our understanding of which crop
                             (application site) activities contribute the greatest to pesticide exceedance days."),
                           
                           hr(),
                           br(),
                           p(strong("Select a watershed from the dropdown menu to view the days of pesticide 
                             concentration exceedance for each taxa in the top chart and by crop 
                             (application site type) in the bottom chart:")),
                           
                           br(),
                           br(),
                           
                           fluidRow(
                             column(3,
                                    # days of exceedance drop down menu for top crops by watershed
                                    wellPanel(
                                      
                                      strong("Days of Exceedance Per Crop"),
                                      br(),
                                      selectInput(
                                        inputId = 'crop_exceedance_select',
                                        label = 'Select a watershed:',
                                        choices = unique(exceed_longer$watersheds)
                                      ) #End SelectInput - crop exceedance dropdown
                                      
                                    ) # end wellPanel - crop exceedance dropdown
                             ), #end column
                             
                             mainPanel(
                               column(12,
                                      # Adding the species plot output from our server
                                      plotlyOutput(outputId = 'crop_exceedance_plot') 
                                      
                               ) # end column  - crop exceedance dropdown
                             ) #end mainPanel  - crop exceedance dropdown
                             
                           ), # end fluidRow  - crop exceedance dropdown
                           
                           br(),
                           br(),
                           br(),
                           
                           fluidRow(
                             column(3,
                                    ## watershed drop down menu for species 
                                    wellPanel(
                                      
                                      strong("Days of Exceedance Per Taxa"),
                                      br(),
                                      selectInput(
                                        inputId = 'watershed_species_select',
                                        label = 'Select a watershed:',
                                        choices = unique(watershed_species_risk$watersheds)
                                      ) # End SelectInput - species exceedance dropdown
                                      
                                    ) # end wellPanel - species exceedance dropdown
                             ), #end column
                             
                             mainPanel(
                               column(12,
                                      # Adding the species plot output from our server
                                      
                                      plotlyOutput(outputId = 'watershed_species_plot')  
                                      
                               ), # end column - species exceedance dropdown
                               
                               br(), 
                               br()
                             ) #end mainPanel - species exceedance dropdown
                             
                           ) # end fluidRow - species exceedance dropdown
                           
                           
                  ) # End tabPanel - species tab
                  
                  
                  #######################################################################################
                  
                ) # End tabsetPanel
) # end fluidPage 


#######################################################################################
### Define server 
#######################################################################################

server <- function(input, output) {
  #######################################################################################
  ## Welcome tab output - Jaenna ----
  
  # Image output
  output$crissy_field <- renderImage({
    
    list(src = "www/crissy_field_3.jpg",
         width = "100%",
         height = 400)
    
  }, deleteFile = F) # end renderImage
  
  # Just using sample output from the widget gallery website for now 
  output$value1 <- renderPrint({ input$select })
  
  
  #######################################################################################
  ## Tab 1 - Map output (pesticide risk by watershed) - Kira ----
  # output$range <- renderPrint({ input$tox_yr_slider }) 
  
  ### Reactive df for Map popup
  risk_annual_perc <- reactive({
    watershed_sf_merge_clean %>% 
    filter(year %in% input$year_map)
  })
  
  ### Reactive df for filtering Map data
  risk_filter <- reactive({
    watershed_sf_merge_clean %>% 
    filter(year %in% input$year_map) %>% 
    filter(index_type %in% input$index_map)
  })
  
  
  
  ### Leaflet map based on year and risk index 
  output$risk_map <- renderLeaflet({
    
    leaflet() %>%
      leaflet::addPolygons(data = risk_filter()) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng = -121.4194, lat = 37.7749, zoom = 8) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
      addPolygons(data = risk_filter(),
                  color = ~map_colors(quartile), weight = 0.6, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.8,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste0("Watershed: ", risk_filter()$name,
                                 "<br>",
                                 "Selected Risk: ", risk_filter()$quartile)) %>% 
      addLegend(colors = c('#320607', '#763b3c', '#a98585', '#ede6e6'),
        labels = c("high", 'moderate', 'low', 'negligible'),
        title = "Risk Severity:", 
        opacity = 1, 
        position = "topright")
      
    #, 
                                 # "<br>",
                                 # "Risk to Aquatic Invertebrates: ", risk_annual_perc()$water_invert_quart, 
                                 # "<br>",
                                 # "Risk to Vascular Plants: ", risk_annual_perc()$plant_v_quart, 
                                 # "<br>", 
                                 # "Risk to Nonvascular Plants: ", risk_annual_perc()$plant_nv_quart,
                                 # "<br>", 
                                 # "Risk to Terrestrial Invertebrates: ", risk_annual_perc()$sed_quart,
                                 # "<br>",
                                 # "Net Pesticide Toxicity Risk: ", risk_annual_perc()$net_quart))
  })
  
  ### Reactive df for watershed 
  watershed_by_yr <- reactive({
    watershed_annual %>% 
      filter(year %in% (input$tox_yr_slider[1]:input$tox_yr_slider[2])) %>% 
      filter(huc %in% c(input$watershed_select)) %>% 
      group_by(year, huc) %>% 
      summarize(totals = sum(RI_net))
  })
  
  ### Plot of selected watershed by year 
  output$watershed_yr_plot <- renderPlotly({ggplot(data = watershed_by_yr(),
                                                   aes(x = year, y = totals, color = huc)) +
      geom_line(size = 1) +
      labs(x = "Date", y = "Overall Risk", color = "Watershed") +
      scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019)) +
      theme_minimal()  +
      scale_color_manual(values = our_colors)
  }) 
  
  
  #######################################################################################
  ## Tab 2 - Application site type - Sadie ----
  #reactive data frame to select application site type
  hru_monthly_df <- reactive ({
    crop_monthly_final %>% 
      filter(hru %in% input$hru_dropdown) %>% 
      filter(year %in% input$year_dropdown) %>% 
      filter(index_type %in% input$index_type_checkboxes) 
  })
  
  #reactive colors in the server for line graph color matching
  color_react_df <- reactive ({
    color_df %>% 
      filter(variable %in% input$index_type_checkboxes)
  })
  
  #plot of monthly data for one selected year
  output$hru_monthly_plot <- renderPlotly({
    ggplot(data = hru_monthly_df(),
           aes(x = date, y = risk_index_value, color = index_type)) +
      geom_line(size = 1) +
      labs(x = "Date", y = "Risk Index", color = "Risk Index Type") +
      ggtitle(paste("Risk indexes for", 
                    input$hru_dropdown,
                    "in",
                    input$year_dropdown)) +
      scale_color_manual(breaks = color_react_df()$variable, values = color_react_df()$color) +
      theme_minimal()
  })

  #filter data frame for annual data (all years)
  hru_annual_df <- reactive ({
    crop_annual %>% 
      filter(hru %in% input$hru_dropdown) %>% 
      filter(index_type %in% input$index_type_checkboxes) 
  })
  
  #plot of all years
  output$hru_annual_plot <- renderPlotly({
    ggplot(data = hru_annual_df(),
           aes(x = year, y = risk_index_value, color = index_type)) +
      geom_line(size = 1) +
      scale_color_manual(breaks = color_react_df()$variable, values = color_react_df()$color) +
      labs(x = "Year", y = "Risk Index", color = "Risk Index Type") +
      ggtitle(paste("Risk indexes for", 
                    input$hru_dropdown,
                    "across all years")) +
      theme_minimal()
  })
  
  #reactive data frame to find top ten crops contributing to risk index for a selected risk index
  top_ten_crops_df <- reactive ({
    crop_annual %>% 
      filter(index_type %in% input$index_top_ten_dropdown) %>% 
      group_by(hru) %>% 
      summarize(mean_ri = mean(risk_index_value, na.rm = TRUE)) %>% 
      slice_max(mean_ri, n = 10)
  })
  
  #top ten crops plot
  output$top_ten_crops <- renderPlotly({
    ggplot(data = top_ten_crops_df(),
           aes(x = fct_reorder(hru, mean_ri), y = mean_ri)) +
      geom_col(fill = "#85d6a5") +
      coord_flip() +
      labs(x = "Application site type", y = "Average risk index across all years") +
      ggtitle(str_wrap(paste("Top ten application site types with the highest risk index for", 
                    input$index_top_ten_dropdown,
                    ""))) +
      theme_minimal() +
      theme(axis.title.y = element_blank())
  })
  

  
  #######################################################################################
  ## Tab 3 - Pesticide risk to animals output - Jaenna ----
 
  # Creating reactive data input for the days of exceedance for every crop per application site
  crop_exceedance_select <- reactive({
    exceed_longer %>%
      select(crop, pesticide, watersheds, days) %>%
      dplyr::filter(watersheds == input$crop_exceedance_select) %>% 
      slice_max(days, n = 15) %>% # keeping the largest values of the counts by day
      mutate(crop = fct_reorder(crop, -days))
  }) # End crop type reactive
  
  
  # Creating bar charts of the days of exceedance for every crop per watershed
  output$crop_exceedance_plot <- renderPlotly({
    ggplot(data = crop_exceedance_select(),
           aes(y = days, x = crop, fill = pesticide)) +
      geom_col(position = "dodge", color = "white", size = 0.6) +
      labs(y = 'Days of Exceedance', x = "Crop (application site type)") + 
      ggtitle(paste("Days of exceedance per crops within", 
                    input$crop_exceedance_select)) +
      scale_color_manual(values = our_colors, aesthetics = "fill") +
      coord_flip() +
      theme_minimal() + theme(axis.title.y = element_blank())
  }) # End crop type reactive plot
  
  
  # Creating reactive data input for the top 5 days of exceedance for every species per application site
  watershed_species_select <- reactive({
    watershed_species_risk %>%
      select(species, pesticide, watersheds, days) %>%
      dplyr::filter(watersheds == input$watershed_species_select) %>% 
      slice_max(days, n = 15) %>% # keeping the largest values of the counts by day
      mutate(species = fct_reorder(species, -days))
  }) # End species application site reactive
  
  
  # Creating bar charts of the days of exceedance for every species per application site type
  output$watershed_species_plot <- renderPlotly({
    ggplot(data = watershed_species_select(),
           aes(y = days, x = species, fill = pesticide)) +
      geom_col(position = "dodge", color = "white", size = 0.6) +
      labs(y = 'Days of Exceedance', x = "Species") + 
      ggtitle(paste("Days of exceedance per taxa within", 
                    input$watershed_species_select)) +
      scale_color_manual(values = our_colors, aesthetics = "fill") +
      coord_flip() + 
      theme_minimal() + theme(axis.title.y = element_blank())
  }) # End species application site reactive plot
  
  
  #######################################################################################
} # end server function 



### Run the application ----
shinyApp(ui = ui, server = server)

