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


#######################################################################################
### Model Output Data
#######################################################################################

#### Tab 1 annual data: annual watershed risk summary
watershed_annual <- read_csv(here("Tab1_Watershed_RiskSummary_Annual.csv"))

watersheds_sf <- read_sf(here::here("spatial_data/BDW_Watersheds/BDW_Near_HUC12.shp")) %>% 
  st_transform('+proj=longlat +datum=WGS84')

rmapshaper::ms_simplify(watersheds_sf)

#add dataframes for map data here with all risk indexes included, grouped by year and watershed, averaged for each year


#######################################################################################
#### Tab 2 annual data: annual crop risk summary
crop_annual <- read_csv(here("Tab2_Crop_RiskSummary_Annual.csv")) %>% 
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value") %>% 
  filter(huc == "All Watersheds")


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
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value") %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date))


#######################################################################################
#### Tab 3 data: days exceeding health benchmarks

## Loading in data and renaming the column names
days_exceed <- read_csv(here("Tab3_Days_ExceedHealthBenchmarks.csv")) %>% 
  rename(fish = days_fish) %>% 
  rename("aquatic invertebrates" = days_invertebrate_water) %>% 
  rename("sediment invertebrates" = days_invertebrate_sed) %>% 
  rename("non-vascular plants" = days_plant_nonvascular) %>% 
  rename("vascular plants" = days_plant_vascular) %>% 
  rename("any species" = days_any_species)  

## Making a longer data frame to work with 
exceed_longer <- days_exceed %>% 
  pivot_longer(cols = fish:"any species", names_to = "species", values_to = "days") %>% 
  rename(watersheds = huc) %>% 
  mutate(pesticide = str_to_lower(pesticide),
         crop = str_to_lower(crop)) 

##  Filtering only the days of exceedance for each individual species - but not "any" species)
watershed_species_risk <- exceed_longer %>% 
  select(species, pesticide, watersheds, days) %>% 
  filter(species != "any species")


#######################################################################################
### Spatial Data
#######################################################################################

# watershed outline shapefile
watershed_shp <- read_sf(here("spatial_data", "BDW_Watersheds", "BDW_Near_HUC12.shp"))


#######################################################################################
### Color Palette
#######################################################################################
# main color: #85d6a5
#### Tab 2 reactive color data frame
color_df <- data.frame(variable = c("RI_net", "RI_fish", "RI_invertebrate_water", "RI_invertebrate_sed", "RI_plant_nonvascular", "RI_plant_vascular"), 
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
                     p("The data used in t his analysis originated from the Environmental Fate Tool which analyzes pesticide risks across the United States. 
                     The Environmental Fate Tool is the second model of the Pesticide Management Prioritization Module (PMPM), 
                     which predicts spatiotemporal explicit concentrations of pesticides from agricultural use in soil, water, and sediment. The use
                     data is compiled from pesticide use reports with data at the daily time-step (required
                     by growers in CA). Pesticide concentrations are predicted using mechanistic models that
                     consider climate, hydrology, irrigation practices, and pesticide properties in the 226 
                     watersheds within ~100 km of the Bay Delta Watershed (22,000 km2)."),
                     br(),
                     p("For the analysis in this website, only pesticide exposure risk and days of exceedance of 
                       pesticide concentration are utilized. Pesticide concentration values are not included in the analysis."),
                     
                     br(),
        
                     p(strong("The PMPM is designed to address limitations of existing tools for pesticide impact
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
                    strong("What are the PMPM goals?"),
                    p("As humans move toward pesticides that are lower in toxicity for mammals, but are orders of magnitude
                     more toxic to invertebrates and aquatic organisms, the PMPM aims to identify:"),
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
                      p("The Bay Delta Watershed. The various colored regions represent the main areas of the watershed.
                                 Photo courtesy of the United States Environmental Protection Agency.",
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
                     p("An interactive map of the pesticide exposure risk (risk index) by watersheds within the San Francisco Bay Delta
                       watershed. The pesticide risk can be analyzed annually with model data from 2015 to 2019. 
                       The pesticide risks selected represent the pesticide risks greater than 95%."), 
                     br(),
  
                     p(strong("Tab 2: Temporal Trends by Application Site Type")), 
                     p("Interactive time series graphs of the pesticide exposure risk (risk index) to fish, invertebrates 
                       (exposure through water or sediment), vascular plants, and nonvascular plants. The graphs are grouped
                       by application site type (crop type). Application site types describe the different types of crops associated with pesticide
                       use in the Bay Delta Watershed. The overall net risk index can also be displayed.
                       The pesticide risk graphs can be analyzed annually with model data from 2015 to 2019. 
                       The pesticide risks selected represent the pesticide risks greater than 95%."), 
                     br(),
                     
                  p(strong("Tab 3: Pesticide Exceedance on Species and Crops")), 
                  p("Interactive bar charts of the modeled number of days a pesticide in water 
                    exceeded the concentration at which severe and adverse effects would occur for various 
                    crops, and aquatic and sediment species. For the purpose of this analysis, only the top 
                    15 counts of days of exceedance were selected for each bar chart. 
                    The model data is from 2015 - 2019. The bar charts are grouped by watershed."), 
                     br(),
                     ), # End column 7
                  
                  column(width=3,
                         br(),
                         tags$img(src="crops.png",width="310px",height="360px", align = "justify"),
                         br(),
                         br(), 
                         p("Plant nursery, Almond orchard, and Christmas tree farm. Various application site types within the Bay Delta Watershed."),
                         br(),
                         style="text-align:justify;color:black, font-size:12px",
                  ) ## End column 6 - fluidrow
                    ), # end fluid row
    
                  
                  fluidRow(
                    ## Data sourcing 
                    column(width=8,
                    h4(strong("Data Source"), style="text-align:justify;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                    p("Data sourced from Nicol Parker, PhD Candidate at the University of California, 
                      Santa Barbara, Bren School of Environmental Science & Management. With support from the 
                      Bay Delta Science Fellowship, and initiative of the California Sea Grant."),
                      
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                     
                  tags$p(HTML("To download the data and userguide, click 
                                <a href=\"https://datadryad.org/stash/share/7a-F-jEXmlvWi3-xeRx_X4osZqXrr8Nh97tnx2bBOSk/\">here.</a>")), 
        
                  br(),
                  br(),
                  br(),
                  hr(), 
                  
                  ## End data source 
                  
                  ## Adding development credits 
                  p(em("Developed by"),br("Kira Archipov, Sadie Cwikiel, and Jaenna Wessling"),style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                  br(),
                  br(),
                 ), # End column 8 fluid row- species photo 
                 
                 
                 ## Species photo column
                 column(width=3,
                        br(),
                        tags$img(src="species.png",width="310px",height="360px", align = "justify"),
                        br(),
                        br(), 
                        p("Polychaete, pricky sculpin, and rock crab. Aquatic and benthic fish and invertebrate species of the Bay Delta Watershed."),
                        br(),
                        style="text-align:justify;color:black, font-size:12px",
                 ), ## End column 4 - species photo column
                 
                  ), # end fluidrow 3
                  
                  #### End fluidrow copied
             
                  ), # End tabPanel - Welcome Page
                  
                  #######################################################################################
                  # Tab 1 - Map tab - Kira ----
       
                  tabPanel("Map of Pesticide Risk", 
                           hr(),
                           ###### Map and map widgets
                           fluidRow(
                             
                             br(),
                             
                             h5("Risk of Pesticide Exposure in the Bay Delta Watershed", style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                             
                             br(),
                             
                             p("TEXT TO EXPLAIN THE MAP."),
                             hr(),
                             br(),
                             
                             ### widgets for map
                             column(3, position = "right",
                                           
                                      #select year
                                      wellPanel(
                                        selectInput('year_map', 
                                                    label = 'Select Year:', 
                                                    choices = unique(watershed_annual$year),
                                                    "2015", 
                                                    multiple = FALSE),
                                        
                                      ), #end year wellPanel
                                           
                                      #select index
                                      wellPanel(
                                        selectInput('index_map', 
                                                    label = 'Select Index Type:', 
                                                    choices = unique(watershed_annual$index)) ## NEED TO MAKE THIS A COLUMN
                                            
                                      ), #end index wellPanel
                                   
                                            
                                    ), # end map widget column
                       
                             
                             ### MAP
                             mainPanel(
                               column(12,
                                      
                                      # Map Title 
                                      tags$strong("Pesticide Risk in Watersheds Surrounding the Bay Delta"), 
                                      
                                      #Leaflet map - NEED TO INCORPORATE REACTIVITY 
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
                                      
                                      ) #end column
                               
                             ) #end mainPanel
                        
                           ), #end fluidRow -- MAP
                           
                           br(),
                           
                           br(),
                           
                           br(),
                        
                          
                           ##### fluidRow for graph and graph widgets
                           fluidRow(
                             
                             p("TEXT TO EXPLAIN THE GRAPH."),
                             
                             column(3,
                                    #widgets for graph 
                                    wellPanel(tags$strong("Overall Pesticide Toxicity Risk Over Time"), 
                                              
                                              selectInput('watershed_select', 
                                                          label = 'Select Watershed(s):', 
                                                          choices = unique(watershed_annual$huc),
                                                          "Antelope Creek", 
                                                          multiple = TRUE), #END Watershed dropdown
                                              
                                              sliderInput("tox_yr_slider", 
                                                          label = h3("Select Year Range:"), 
                                                          min = 2015, 
                                                          max = 2019, value = c(2016, 2018), 
                                                          sep = "") #END year slider 
                                              
                                    ), # END wellPanel - Map tab
                                    
                              ), #end column
                             
                           
                             ### Graph mainPanel
                             mainPanel(
                               column(12,
                                      tags$strong("Overall Pesticide Toxicity Risk by Watershed"),
                                      
                                      plotlyOutput(outputId = 'watershed_yr_plot')   
                                 
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
                           fluidRow(
                             
                             br(),
                             
                             h5("The Pesticide Exposure Risk Index for Plants and Invertebrates for Different Application Site Types ", style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                             
                             br(),
                          
                             p("Application site types describe the different types of crops associated with pesticide use in the Bay Delta Watershed. The figures below show the pesticide exposure risk (risk index) to fish, invertebrates (exposure through water or sediment), vascular plants, and
                                        nonvascular plants. The overall net risk index can also be displayed."),
                             br(),
                             
                             p("NEED TO EXPLAIN THE FIGURES. Select which application site type (crop type) to display the risk indices for the different categories of plants and animals, and select which risk indices to display.
                                        Figure 1 shows .... la la la."),
                
                             p("Application site types describe the different croplands associated with pesticide use in the Bay Delta Watershed. Different amounts and types of pesticides are applied to each type of crop. 
                             The figures below show the pesticide exposure risk (risk index) to fish, invertebrates (in water or benthic sediment), vascular plants, nonvascular plants, and the overall net risk index. 
                             The net risk index is the risk index observed for all species evaluated, summarized across all species."),

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
                                                         selected = "RI_net") #end risk index checkboxes
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
                           
                           fluidRow(
                             column(3,
                                    # risk index dropdown for top ten crop figures (does not impact line graphs)
                                    wellPanel(
                                      strong("Risk Index Type"),
                                      selectInput("index_top_ten_dropdown",
                                                  label = "Pick a risk index type",
                                                  choices = unique(crop_annual$index_type)) #end risk dropdown
                                    ) # end wellPanel
                              ), #end column
                             
                             mainPanel(
                                    column(12,
                                           
                                           #Figure 3: top ten crops that contribute to risk for each index
                                           p("The figure below shows which ten application site types contribute the most pesticide exposure risk for each index type.
                                             Select which application site type (crop type) to display."),
                                       
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
                  tabPanel("Pesticide Exceedance on Species and Crops",
                           hr(),
                           h5("Daily Pesticide Exceedance on Species and Crops", style="text-align:center;color:black;background-color:#85d6a5;padding:15px;border-radius:10px"),
                           p(strong("How does pesticide concentration exceedance differ between animals, 
                                    plants, and crops? Does it differ by application site type?")),
                           
                           p("The figures below illustrate the modeled number of days a pesticide in water
                             exceeded the concentration at which severe and adverse effects would occur for
                             various crops, and aquatic and sediment species. \nFor the purpose of this 
                             analysis, only the top 15 counts of days of exceedance were selected for 
                             each bar chart."),
                 
                           hr(),
                           p(strong("Select a watershed from the dropdown menu to view the days of pesticide 
                             concentration exceedance for species in the top chart and by crop 
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
                                        label = 'Select a watershed',
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
                                      
                                      strong("Days of Exceedance Per Species"),
                                      br(),
                                      selectInput(
                                        inputId = 'watershed_species_select',
                                        label = 'Select a watershed',
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
      theme_minimal()
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
      ggtitle(paste("Risk Indexes for", 
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
      ggtitle(paste("Risk Indexes for", 
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
      labs(x = "Average risk index across all years", y = "Application site type") +
      ggtitle(paste("Top Ten Application Site Types for", 
                    input$index_top_ten_dropdown)) +
      theme_minimal()
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
      ggtitle(paste("Days of crop exceedance within", 
                    input$crop_exceedance_select)) +
      scale_color_manual(values = our_colors, aesthetics = "fill") +
      coord_flip() +
      theme_minimal()
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
      ggtitle(paste("Days of species exceedance within", 
                    input$watershed_species_select)) +
      scale_color_manual(values = our_colors, aesthetics = "fill") +
      coord_flip() + 
      theme_minimal()
  }) # End species application site reactive plot
  
  
  #######################################################################################
} # end server function 



### Run the application ----
shinyApp(ui = ui, server = server)

