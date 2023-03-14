#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(leaflet)
library(bslib) 
library(vroom) 
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

# Bootstrapping library to make the Shiny App look even cooler
# ?bs_theme() put in console to see what we can do 

#########################################################
## Data wrangling

## Loading in data and renaming the column names
days_exceed <- read_csv(here("Tab3_Days_ExceedHealthBenchmarks.csv")) %>% 
  rename(fish = days_fish) %>% 
  rename("aquatic invertebrates" = days_invertebrate_water) %>% 
  rename("sediment invertebrates" = days_invertebrate_sed) %>% 
  rename("non-vascular plants" = days_plant_nonvascular) %>% 
  rename("vascular plants" = days_plant_vascular) %>% 
  rename("any species" = days_any_species)  

# Making a longer data frame to work with 
exceed_longer <- days_exceed %>% 
  pivot_longer(cols = fish:"any species", names_to = "species", values_to = "days") %>% 
  rename(application_sites = huc) %>% 
  mutate(pesticide = str_to_lower(pesticide),
         crop = str_to_lower(crop)) 


# Filtering only the days of exceedance for each individual species - but not "any" species)
app_site_species_risk <- exceed_longer %>% 
  select(species, pesticide, application_sites, days) %>% 
  filter(species != "any species") 


### Creating a vector version of this, not connected to specific variables
our_colors = c("#85d6a5", "#00796f", "#DBA507", "#CC7354", "#8EC7D2", "#d0c1db", "#355C7F", "#A23E49",
                        "#4d3591", "#966E5C", "#9B945F", "#ADDFB3", "#F2ACB9", "#A8A9AD", "#483C32",
                        "#BBECF2", "#540B0C")


##########################################################



my_theme <- bs_theme(
  bootswatch = "minty") 


# Define UI ---- 
ui <- fluidPage(theme = my_theme,
                
                # Application title
                titlePanel("The Pesticide Management Prioritization Module (PMPM)"),
                
                
                # Adding our tabs panel
                tabsetPanel(
                  
                  
                  # Tab 1 - Welcome Tab - Jaenna ----
                  tabPanel(icon("home"),
                           
                           # Adding an image to the front page
                           imageOutput("sf_news"),
                           
                           # Adding text beneath photo for credits
                           p(em("Egret in the San Francisco Bay Delta Watershed. (Photo courtesy of SF News.)"), style="text-align: center; font-size:12px"
                           ), # end photo text
                           
                           hr(), # horizontal line break
                           
                           
                           # Creating a fluid row to create multiple columns to have information and a small photo
                           fluidRow(
                             column(
                               br(),
                               tags$img(src="sf_news.jpeg",width="250px",height="310px", align = "justify"),
                               br(),
                               p("The Bay Delta Watershed. The various colored regions represent the main areas of the watershed.
                                 Photo courtesy of the United States Environmental Protection Agency.",
                                 br(),
                                 style="text-align:justify;color:black, font-size:12px"),
                               width=2),
                             
                             br(),
                             column(width=8,
                                    
                                    h4(strong("Purpose"), style="text-align:justify;color:black;background-color:lightgreen;padding:15px;border-radius:10px"),
                                    p("This interactive tool illustrates the daily predicted pesticide concentrations and risk
                     based on toxicity to fish, aquatic invertebrates, aquatic nonvascular plants (algae), 
                     and aquatic vascular plants in the Bay Delta Watershed."), # End paragraph 1 
                     br(), # Line break    
                     
                     h3(strong("Background"), style="text-align:justify;color:black;background-color:lightgreen;padding:15px;border-radius:10px"),
                     p("The Pesticide Management Prioritization Module (PMPM) predicts spatiotemporal explicit 
                     concentrations of pesticides from agricultural use in soil, water, and sediment. The use
                     data is compiled from pesticide use reports with data at the daily time-step (required
                     by growers in CA). Pesticide concentrations are predicted using mechanistic models that
                     consider climate, hydrology, irrigation practices, and pesticide properties in the 226 
                     watersheds within ~100 km of the Bay Delta Watershed (22,000 km2). 13% of California’s 
                     waterways are designated as impaired by pesticides of those assessed for non-point 
                     source pollution under the Clean Water Act. 56% are present within the Bay Delta 
                     Watershed (BDW), home to over 90 threatened and endangered species. As humans move
                     toward pesticides that are lower in toxicity for mammals, but are orders of magnitude
                     more toxic to invertebrates and aquatic organisms, the PMPM aims to identify 
                     1) Which activities are imposing the greatest pesticide loads? 
                     2) Who is responsible? 
                     3) How can tradeoffs between the benefits of chemical use be managed to restore
                     and preserve ecosystem health?") # end paragraph 2
                     
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
        
                  # Species tab - Jaenna ----
                  tabPanel("Pesticide Exceedance on Species and Crops",
                           
                           br(),
                           
                           fluidRow(
                             column(3,
                                    # days of exceedance drop down menu for top crops by application sites 
                                    wellPanel(
                                      
                                      strong("Days of Exceedance Per Crop"),
                                      selectInput(
                                        inputId = 'crop_exceedance_select',
                                        label = 'Select application site',
                                        choices = unique(exceed_longer$application_sites)
                                      ), #End SelectInput - crop exceedance dropdown
                                      
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
                           
                           fluidRow(
                             column(3,
                                    # risk index dropdown for top ten crop figures (does not impact line graphs)
                                    wellPanel(
                                      
                                      strong("Days of Exceedance Per Species"),
                                      selectInput(
                                        inputId = 'app_site_species_select',
                                        label = 'Select application site',
                                        choices = unique(app_site_species_risk$application_sites)
                                      ) # End SelectInput - application site exceedance dropdown
                                      
                                    ) # end wellPanel - application site exceedance dropdown
                             ), #end column
                             
                             mainPanel(
                               column(12,
                                      # Adding the application site plot output from our server
                                      
                                      plotlyOutput(outputId = 'app_site_species_plot')  
                                      
                               ), # end column - application site exceedance dropdown
                               
                               br(), 
                               br()
                             ) #end mainPanel - application site exceedance dropdown
                             
                           ) # end fluidRow - application site exceedance dropdown
                           
                ) # End tabPanel - species tab
  
  #######################################################################################
  
) # End tabsetPanel
) # end fluidPage 



# Define server ----
server <- function(input, output) {
  
  # Tab 1 - Welcome output - Jaenna ----
  
  # Top of the page - Image output
  output$sf_news <- renderImage({
    
    list(src = "www/sf_news.jpeg",
         width = "100%",
         height = 400)
    
  }, deleteFile = F) # end renderImage
  
  
  #######################################################################################
  
  # Creating reactive data input for the days of exceedance for every crop per application site
  crop_exceedance_select <- reactive({
    exceed_longer %>%
      select(crop, pesticide, application_sites, days) %>%
      dplyr::filter(application_sites == input$crop_exceedance_select) %>% 
      slice_max(days, n = 15) %>% # keeping the largest values of the counts by day
      mutate(crop = fct_reorder(crop, -days))
  }) # End species watershed reactive
  
  
  # Creating bar charts of the days of exceedance for every crop per application site type
  output$crop_exceedance_plot <- renderPlotly({
    ggplot(data = crop_exceedance_select(),
           aes(y = days, x = crop, fill = pesticide)) +
      geom_col(position = "dodge", color = "white", size = 0.6) +
      labs(y = 'Days of Exceedance', x = "Crop") + 
      ggtitle(paste("Days of crop exceedance within", 
                    input$crop_exceedance_select)) +
      scale_color_manual(values = our_colors, aesthetics = "fill") +
      coord_flip() +
      theme_minimal()
  }) # End watershed reactive plot
  
  
  # Creating reactive data input for the top 5 days of exceedance for every species per application site
  app_site_species_select <- reactive({
    app_site_species_risk %>%
    select(species, pesticide, application_sites, days) %>%
      dplyr::filter(application_sites == input$app_site_species_select) %>% 
      slice_max(days, n = 15) %>% # keeping the largest values of the counts by day
      mutate(species = fct_reorder(species, -days))
  }) # End species watershed reactive
  
  
  # Creating bar charts of the days of exceedance for every species per application site type
  output$app_site_species_plot <- renderPlotly({
    ggplot(data = app_site_species_select(),
           aes(y = days, x = species, fill = pesticide)) +
      geom_col(position = "dodge", color = "white", size = 0.6) +
      labs(y = 'Days of Exceedance', x = "Species") + 
      ggtitle(paste("Days of species exceedance within", 
                    input$app_site_species_select)) +
      scale_color_manual(values = our_colors, aesthetics = "fill") +
      coord_flip() +
      theme_minimal()
  }) # End watershed reactive plot

  
  #######################################################################################
  
} # end server function 



# Run the application ----
shinyApp(ui = ui, server = server)

