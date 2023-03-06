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
library(bslib) # Bootstrapping library to make the Shiny App look even cooler
# ?bs_theme() put in console to see what we can do 


#######################################################################################
### Model Output Data
#######################################################################################

# Tab 1 annual data: annual watershed risk summary
watershed_annual <- read_csv(here("Tab1_Watershed_RiskSummary_Annual.csv"))

watersheds_sf <- read_sf(here::here("spatial_data/BDW_Watersheds/BDW_Near_HUC12.shp")) %>% 
  st_transform('+proj=longlat +datum=WGS84')

rmapshaper::ms_simplify(watersheds_sf)


#######################################################################################
# Tab 2 annual data: annual crop risk summary
crop_annual <- read_csv(here("Tab2_Crop_RiskSummary_Annual.csv")) %>% 
  pivot_longer(RI_fish:RI_net, names_to = "index_type", values_to = "risk_index_value") %>% 
  filter(huc == "All Watersheds")


# Tab 2 monthly data: monthly crop risk summary
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
# Tab 3 data: days exceeding health benchmarks
days_exceed <- read_csv(here("Tab3_Days_ExceedHealthBenchmarks.csv"))

exceed_longer <- days_exceed %>% 
  pivot_longer(cols = days_fish:days_any_species, names_to = "species", values_to = "days") 


#######################################################################################
### Spatial Data
#######################################################################################

# watershed outline shapefile
watershed_shp <- read_sf(here("spatial_data", "BDW_Watersheds", "BDW_Near_HUC12.shp"))



### Theme
my_theme <- bs_theme(
  bootswatch = "minty") 
 

### Define UI ---- 
ui <- fluidPage(theme = my_theme, 
                
                # Application title
                titlePanel("The Pesticide Management Prioritization Module (PMPM)"),
                
                # Adding our tabs panel
                tabsetPanel(
                  #######################################################################################
                  # Welcome Tab - Jaenna ----
                  tabPanel(icon("home"),
                           
                           # Adding an image to the front page
                           imageOutput("sf_news"),
                           
                           # Adding text beneath photo for credits
                           p(em("Egret in the San Francisco Bay Delta Watershed. (Photo courtesy of SF News.)"), style="text-align: center; font-size:12px"
                           ), # end photo text
                           
                           hr(), # horizontal line break
                           
                           
                           # Creating a fluid row to can create multiple columns to have information and a small photo
                           fluidRow(
                             column(
                               br(),
                               tags$img(src="watershed.jpg",width="200px",height="260px", align = "justify"),
                               br(),
                               p("The Bay Delta Watershed. The various colored regions represent the main areas of the watershed.
                                 Photo courtesy of the United States Environmental Protection Agency.",
                                 br(),
                                 style="text-align:justify;color:black, font-size:12px"),
                               width=3),
                             
                             br(),
                             column(width=8,
                                    
                                    h4(strong("Purpose"), style="text-align:justify;color:black;background-color:lightgreen;padding:15px;border-radius:10px"),
                                    p("This interactive tool illustrates the daily predicted pesticide concentrations and risk
                     based on toxicity to fish, aquatic invertebrates, aquatic nonvascular plants (algae), 
                     and aquatic vascular plants in the (San Francisco) Bay Delta Watershed."), # End paragraph 1 
                     br(), # Line break    
                     
                     h3(strong("Background"), style="text-align:justify;color:black;background-color:lightgreen;padding:15px;border-radius:10px"),
                     strong("What does the PMPM do?"),
                     p("The Pesticide Management Prioritization Module (PMPM) predicts spatiotemporal explicit 
                     concentrations of pesticides from agricultural use in soil, water, and sediment. The use
                     data is compiled from pesticide use reports with data at the daily time-step (required
                     by growers in CA). Pesticide concentrations are predicted using mechanistic models that
                     consider climate, hydrology, irrigation practices, and pesticide properties in the 226 
                     watersheds within ~100 km of the Bay Delta Watershed (22,000 km2)."),
                     
                     br(),
                     strong("Why is this analysis needed?"),
                     p("13% of California’s 
                     waterways are designated as impaired by pesticides of those assessed for non-point 
                     source pollution under the Clean Water Act. 56% are present within the Bay Delta 
                     Watershed (BDW), home to over 90 threatened and endangered species."),
                     
                     br(),
                     strong("What are the PMPM goals?"),
                     p("As humans move
                     toward pesticides that are lower in toxicity for mammals, but are orders of magnitude
                     more toxic to invertebrates and aquatic organisms, the PMPM aims to identify:"),
                     p("1) Which activities are imposing the greatest pesticide loads?"),
                     p("2) Who is responsible?"),
                     p("3) How can tradeoffs between the benefits of chemical use be managed to restore
                     and preserve ecosystem health?") # end of background section 
                     
                             ) # end column 1 
                           ), # end fluidrow  
                     
                     #### End fluidrow copied
                     
                     
                     # Adding text and output to the main panel
                     mainPanel(
                       
                       hr(),
                       
                       # Data sourcing 
                       h3(strong("Data Source"), style="text-align:justify;color:black;background-color:lightgreen;padding:15px;border-radius:10px"),
                       p("Data sourced from Nicol Parker, PhD Candidate University of California, 
                      Santa Barbara, Bren School of Environmental Science & Management. With support from the 
                      Bay Delta Science Fellowship, and initiative of the California Sea Grant."), 
                      
                      br(),
                      
                      
                      tags$p(HTML("To download the data and userguide, click 
                                <a href=\"https://datadryad.org/stash/share/7a-F-jEXmlvWi3-xeRx_X4osZqXrr8Nh97tnx2bBOSk/\">here.</a>")), 
                      
                      hr(), 
                      # End data source 
                      
                      # Adding development credits 
                      p(em("Developed by"),br("Kira Archipov, Sadie Cwikiel, and Jaenna Wessling"),style="text-align:center;color:black;background-color:lightgreen;padding:15px;border-radius:10px")
                     ) # End mainPanel - Welcome page
                  ), # End tabPanel - Welcome Page
                  
                  #######################################################################################
                  # Tab 1 - Map tab - Kira ----
                  tabPanel("Map of Pesticide Risk", 
                           sidebarLayout(position = "right",
                                         
                                         sidebarPanel(
                                           tags$strong("Overall Pesticide Toxicity Risk Over Time"), 
                                           
                                           sliderInput("tox_yr_slider", label = h3("Select Year Range:"), min = 2010, 
                                                       max = 2020, value = c(2012, 2019), # NEED TO confirm year range when we get data
                                                       sep = ""), 
                                           
                                           "PLACEHOLDER: Graph showing total tox levels over time, that changes with slider"
                                        
                                         ), # END sidebar panel - Map tab
                                         
                                         
                                         mainPanel(
                                           
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
                                           
                                         ), #END main panel - map tab
                                         
                           ) # END sidebarLayout - map tab
                           
                  ), # END tabPanel - map
                
                  
                  #######################################################################################
                  # Tab 2 - Application site type (crop) data - Sadie ----
                  tabPanel("Temporal Trends by Application Site Type", 
                           sidebarLayout(
                             #dropdown menus
                             sidebarPanel(strong("Application Site Type"),
                                          #dropdown menu for application site type
                                          selectInput("hru_dropdown",
                                                      label = "Select an application site type (crop type)",
                                                      choices = unique(crop_monthly_final$hru)), #end pesticide dropdown
                                          
                                          br(),
                                          
                                          #dropdown menu for watershed 
                                          # "Watersheds",
                                          # selectInput("watershed_dropdown",
                                          #             label = "Select watershed",
                                          #             choices = unique(crop_monthly_final$huc)), #end watershed dropdown
                                          
                                          #dropdown menu for year 
                                          strong("Year"),
                                          selectInput("year_dropdown",
                                                      label = "Select year",
                                                      choices = unique(crop_monthly_final$year)), #end year dropdown
                                          
                                          br(),
                                          
                                          #checkboxes for risk index 
                                          strong("Risk Index Type"),
                                          checkboxGroupInput("index_type_checkboxes",
                                                      label = "Select risk index type(s)",
                                                      choices = unique(crop_monthly_final$index_type),
                                                      selected = "RI_net"), #end risk index checkboxes
                                          
                                          br(),
                                          
                                          br(),
                                          
                                          br(),
                                          
                                          br(),
                                          
                                          br(),
                                          
                                          br(),
                                          
                                          # new index dropdown for top ten crop figures
                                          strong("Top Ten Crops with Highest Risk Index"),
                                          selectInput("index_top_ten_dropdown",
                                                      label = "Pick a risk index type",
                                                      choices = unique(crop_annual$index_type))
                                          
                                          
                                          ), #end sidebarPanel
                             
                      
                             #display  the graph of temporal trends for the selected pesticide and watershed
                             mainPanel(strong("Temporal trends by Application Site Type in Selected Year"),
                                       plotlyOutput(outputId = 'hru_monthly_plot'), #tell the app where to put the graph
                                       
                                       br(), 
                                       
                                       strong("Temporal trends by Application Site Type for All Years"),
                                       plotlyOutput(outputId = 'hru_annual_plot'),
                                       
                                       br(),
                                       
                                       br(),
                                       
                                       br(),
                                       
                                       br(),
                                       
                                       br(),
                                       
                                       br(),
                                       
                                       strong("Top Ten Application Site Types with the Highest Average Risk Index for All Years"),
                                       plotlyOutput(outputId = 'top_ten_crops')

                                       
                             ) #end mainPanel
                             
                             
                           ) #end sidebarLayout        
                  ), #end tabPanel - temporal trends by application site type
                  
                  #######################################################################################
                  # Tab 3 - Animals tab - Jaenna ----
                  
                  # Animals tab - Jaenna ----
                  tabPanel("Pesticide Impact on Species",
                           sidebarLayout(
                             sidebarPanel("Widget",
                                          selectInput(
                                            inputId = 'species_select',
                                            label = 'Select species',
                                            choices = c('days_fish',
                                                        'days_invertebrate_water', 
                                                        'days_invertebrate_sed', 
                                                        'days_plant_nonvascular',
                                                        'days_plant_vascular', 
                                                        'days_any_species'))
                             ), # end sidebarPanel widgets - Animals tab
                             
                             
                             mainPanel(strong("OUTPUT"), # Subheader
                                       # Adding the output from our server
                                       plotlyOutput(outputId = 'species_plot') 
                             ) # End main panel - Animals tab
                           ) # End sidebarLayout - Animals tab
                  ) # End tabPanel - Animals tab
                  
                  
                  
                ) # End tabsetPanel
) # end fluidPage 



### Define server ----
server <- function(input, output) {
  #######################################################################################
  ## Welcome tab output - Jaenna ----
  
  # Image output
  output$sf_news <- renderImage({
    
    list(src = "www/sf_news.jpeg",
         width = "100%",
         height = 400)
    
  }, deleteFile = F) # end renderImage
  
  # Just using sample output from the widget gallery website for now 
  output$value1 <- renderPrint({ input$select })
  
  #######################################################################################
  ## Tab 1 - Map output (pesticide risk by watershed) - Kira ----
  output$range <- renderPrint({ input$tox_yr_slider }) #PLACEHOLDER - will change with graph 
  
  #######################################################################################
  ## Tab 2 - Application site type - Sadie ----
  #reactive data frame to select application site type
  hru_monthly_df <- reactive ({
    crop_monthly_final %>% 
      filter(hru == input$hru_dropdown) %>% 
      #filter(huc == input$watershed_dropdown) %>% 
      filter(year == input$year_dropdown) %>% 
      filter(index_type == input$index_type_checkboxes) #%>% 
      # group_by(year == input$year_dropdown) %>% 
      # summarize()
  })
  
  #plot of monthly data for one selected year
  output$hru_monthly_plot <- renderPlotly({
    ggplot(data = hru_monthly_df(),
           aes(x = date, y = risk_index_value, color = index_type)) +
      geom_line(size = 2) +
      labs(x = "Date", y = "Risk Index", fill = "Risk Index Type") +
      theme_minimal()
  })
  
  #filter data frame for annual data (all years)
  hru_annual_df <- reactive ({
    crop_annual %>% 
      filter(hru == input$hru_dropdown) %>% 
      #filter(huc == input$watershed_dropdown) %>% 
      filter(index_type == input$index_type_checkboxes) 
  })
  
  #plot of all years
  output$hru_annual_plot <- renderPlotly({
    ggplot(data = hru_annual_df(),
           aes(x = year, y = risk_index_value, color = index_type)) +
      geom_line(size = 2) +
      labs(x = "Year", y = "Risk Index", fill = "Risk Index Type") +
      theme_minimal()
  })
  
  top_ten_crops_df <- reactive ({
    crop_annual %>% 
      filter(index_type == input$index_top_ten_dropdown) %>% 
      group_by(hru) %>% 
      summarize(mean_ri = mean(risk_index_value, na.rm = TRUE)) %>% 
      slice_max(mean_ri, n = 10)
  })
  
  output$top_ten_crops <- renderPlotly({
    ggplot(data = top_ten_crops_df(),
           aes(x = fct_reorder(hru, mean_ri), y = mean_ri)) +
      geom_col(fill = "turquoise") +
      coord_flip() +
      labs(x = "Average risk index across all years", y = "Application site type") +
      theme_minimal()
  })
  

  
  #######################################################################################
  ## Tab 3 - Pesticide risk to animals output - Jaenna ----
  # Just using sample output from the widget gallery website for now 
  
  
  ## Creating data set for reactive input for species selection
  species_select <- reactive({
    exceed_longer %>%
      select(species, pesticide, huc, days) %>%
      dplyr::filter(species == input$species_select) %>%
      slice_max(days, n = 10) %>% # keeping the largest values of the counts by lake
      arrange(-days) # arranges selected choices from greatest to least
  }) # End species select reactive
  
  
  # Creating a plot using our penguin data
  output$species_plot <- renderPlotly({
    ggplot(data = species_select(),
           aes(y = days, x = reorder(huc, -days), fill = pesticide)) +
      geom_col() +
      labs(y = 'Days of Exceedance', x = "Watershed",
           title = "Greatest Days of Exceedance per Species") +
      theme(axis.text.x = element_text(angle =75, hjust = 1))
  }) # End species reactive plot
  

  
  #######################################################################################
} # end server function 



### Run the application ----
shinyApp(ui = ui, server = server)

