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
library(leaflet)
library(bslib) # Bootstrapping library to make the Shiny App look even cooler
# ?bs_theme() put in console to see what we can do 

### TEMPORARY  DATA -- need to change instances where this is called later on to the relevant dataset that are now read in below
# Reading in our example data (just temporary to practice until we get the real data set)
pesticides <- read_excel(here('Example_Output_DataTable.xlsx')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower)) # changing the characters to lower case 
# View(pesticides) # can uncomment this if you want to view the temporary data 
# Should I try to remove the numbers and letters before each pesticide name, or is it part of the name? 



### Model Output Data

# full model output data set broken down by watershed, application site type, and pesticide
watershed_site_pesticide_df <- read_csv(here('model_output_data', 'BDW_NearHUC12_2015_2019_Watershed_Site_Pesticide_RI.csv')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower))

# model output broken down by application site type and pesticide
site_pesticide_df <- read_csv(here('model_output_data', 'BDW_NearHUC12_2015_2019_Site_Pesticide_RI.csv')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower))

# model output summarized by pesticide
pesticide_df <- read_csv(here('model_output_data', 'BDW_NearHUC12_2015_2019_Pesticide_RI.csv')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower))

# model output summarized by application site type
site_df <- read_csv(here('model_output_data', 'BDW_NearHUC12_2015_2019_Site_RI.csv')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower))

# model output summarized by watershed
watershed_df <- read_csv(here('model_output_data', 'BDW_NearHUC12_2015_2019_Watershed_RI.csv')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower))



### Spatial Data




### Theme
my_theme <- bs_theme(
  bootswatch = "minty") 
 

### Define UI ---- 
ui <- fluidPage(theme = my_theme, 
                
                # Application title
                titlePanel("The Pesticide Management Prioritization Module (PMPM)"),
                
                # Adding our tabs panel
                tabsetPanel(
                  
                  # Tab 1 - Welcome Tab - Jaenna ----
                  tabPanel(icon("home"),
                           
                           # Adding an image to the front page
                           imageOutput("crissy_field"),
                           
                           # Adding text beneath photo for credits
                           p("Photo of Crissy Field: San Francisco, California.", 
                             em("(Photo by Will Elder, 
                             courtesy of the National Park Service)"), style="text-align: center; font-size:14px"
                           ), # end photo text
                           
                           hr(), # horizontal line break
                           
                           
                           # Creating a fluid row to can create multiple columns to have information and a small photo
                           fluidRow(
                             column(
                               br(),
                               tags$img(src="watershed.jpg",width="200px",height="260px", align = "center"),
                               br(),
                               p("The main regions of the (San Francisco) Bay Delta Watershed.", 
                                 em("(Photo courtesy of the United States Environmental Protection Agency.)")),
                                 br(),
                                 style="text-align:center;color:black; font-size: 14px; nowrap = FALSE",
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
                  
                  
                  
                  
                  # Tab 2 - Creating another tab for application site type - Jaenna ----
                  tabPanel("Application Site Type",
                           
                           # Creating sidebar widget first 
                           sidebarLayout(
                             sidebarPanel("WIDGET",
                                          selectInput(
                                            "select", 
                                            label = h3("Select application site type"), 
                                            choices = c("Nursery", "Almond Tree Orchard", "Squash Farm"), 
                                            selected = 1) # end selectInput
                             ), # end sidebarPanel widgets - Application site tab
                             
                             # Adding text and ouput to the main panel
                             mainPanel(
                               
                               # Adding the output from our server (temporary - need to add in the real function later)
                               h3(strong("OUTPUT")), # Subheader
                               "output$value1" # Temporary function
                             ) # End mainPanel - application site tab
                           ) # end sidebarLayout - application site tab 
                  ), # End tabPanel - application site tab
                  
                  # Tab 3 - Map tab - Kira ----
                  tabPanel("Map of Pesticide Risk", 
                           sidebarLayout(position = "right",
                                         
                                         sidebarPanel(
                                           tags$strong("Pesticide Toxicity Over Time"), 
                                           
                                           sliderInput("tox_yr_slider", label = h3("Year(s)"), min = 2010, 
                                                       max = 2020, value = c(2012, 2019), # NEED TO confirm year range when we get data
                                                       sep = ""), 
                                           
                                           "PLACEHOLDER: Graph shoing total tox levels over time, that changes with slider"
                                        
                                         ), # END sidebar panel - Map tab
                                         
                                         
                                         mainPanel(
                                           
                                           # Map Title 
                                           
                                           tags$strong("Pesticide Risk in Watersheds Surrounding the Bay Delta"), 
                                           
                                           #Leaflet map - NEED TO INCORPORATE REACTIVITY 
                                           
                                           leaflet() %>% 
                                             addProviderTiles("Esri.WorldTopoMap") %>% 
                                             setView(lng = -121.4194, lat = 37.7749, zoom = 8) %>% 
                                             addMiniMap(toggleDisplay = TRUE, minimized = TRUE)
                                           
                                         ), #END main panel - map tab
                                         
                           ) # END sidebarLayout - map tab
                           
                  ), # END tabPanel - map
                  
                  # Tab 4 - Temporal trends by pesticide tab - Sadie ----
                  tabPanel("Temporal Trends by Crop",
                           sidebarLayout(
                             #dropdown menus
                             sidebarPanel("Pesticides",
                                          #dropdown  menu for pesticide type
                                          selectInput("pesticide_dropdown",
                                                      label = "Select pesticide",
                                                      choices = unique(pesticides$pesticide)), #end pesticide dropdown
                                          
                                          #dropdown menu for watershed 
                                          "Watersheds",
                                          selectInput("watershed_dropdown",
                                                      label = "Select watershed",
                                                      choices = unique(pesticides$watershed)) #end watershed dropdown
                                          ), #end sidebarPanel
                             #display  the graph of temporal trends for the selected pesticide and watershed
                             mainPanel("Graph  of temporal trends by pesticide and watershed",
                                       plotOutput(outputId = 'pesticide_plot') #tell the app where to put the graph
                             ) #end mainPanel
                           ) #end sidebarLayout        
                  ), #end tabPanel - temporal trends by crop
                  
  
                  # Tab 5 - Animals tab - Jaenna ----
                  tabPanel("Pesticide Impact on Animals",
                           sidebarLayout(
                             sidebarPanel("WIDGET",
                                          selectInput(
                                            "select", 
                                            label = h3("Select animal species"), 
                                            choices = list("Animal 1" = 1, "Animal 2" = 2, "Animal 3" = 3, "Animal 4" = 4, "Animal 5" = 5), 
                                            selected = 1)
                             ), # end sidebarPanel widgets - Animals tab
                             
                             mainPanel(
                               # Adding the output from our server (temporary - need to add in the real function later)
                               strong("OUTPUT"), # Subheader
                               "output$value2") # Temporary function
                           ) # End sidebarLayout - Animals tab
                  ) # End tabPanel - Animals tab
                  
                ) # End tabsetPanel
) # end fluidPage 



### Define server ----
server <- function(input, output) {
  
  ## Tab 1 - Welcome output - Jaenna ----
  
  # Image output
  output$crissy_field <- renderImage({
    
    list(src = "www/crissy_field_3.jpg",
         width = "100%",
         height = 400)
    
  }, deleteFile = F) # end renderImage
  
  # Just using sample output from the widget gallery website for now 
  output$value1 <- renderPrint({ input$select })
  
  ## Tab 2 - Map output (pesticide risk by watershed) - Kira ----
  output$range <- renderPrint({ input$tox_yr_slider }) #PLACEHOLDER - will change with graph 
  
  ## Tab 3 - Pesticide risk by application site type - Sadie ----
  #reactive data frame to select pesticide and watershed
  pesticide_watershed_df <- reactive ({
    pesticides %>% 
      filter(pesticide == input$pesticide_dropdown) %>% 
      filter(watershed == input$watershed_dropdown)
     # group_by() %>% 
     # summarize()
  })
  
  #render plot of pesticide for a watershed (this should probably be a plot of pesticides for a selected application site type?)
  output$pesticide_plot <- renderPlot({
    ggplot(data = pesticides_watershed_df(),
           aes(x = year, y = pesticide_concentration)) +
      geom_col() 
  })
  
  ## Tab 4 - Pesticide risk to animals output - Jaenna ----
  # Just using sample output from the widget gallery website for now 
  output$value2 <- renderPrint({ input$select })
  
} # end server function 



### Run the application ----
shinyApp(ui = ui, server = server)

