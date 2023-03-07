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

# Bootstrapping library to make the Shiny App look even cooler
# ?bs_theme() put in console to see what we can do 


days_exceed <- read_csv(here("Tab3_Days_ExceedHealthBenchmarks.csv"))

# Making days exceed longer so I can use the filter feature in the server output for graphs 
exceed_longer <- days_exceed %>% 
  pivot_longer(cols = days_fish:days_any_species, names_to = "species", values_to = "days") 


# Filtering only the days where exceedance is greater than 0 (so there are no blank spots on the graph)
watershed_species_risk <- exceed_longer %>% 
  select(species, pesticide, huc, days) %>% 
  filter(days > 0, species != "days_any_species")




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
                               tags$img(src="sf_news.jpeg",width="200px",height="260px", align = "justify"),
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
                  
                  
                  
                  # Species tab - Jaenna ----
                  tabPanel("Pesticide Impact on Species",
                           
                           sidebarLayout(
                             sidebarPanel("Widget",
                                          selectInput(
                                            inputId = 'species_select',
                                            label = 'Select species',
                                            choices = unique(exceed_longer$species)),
                                          
                                          selectInput(
                                            inputId = 'watershed_species_select',
                                            label = 'Select watershed',
                                            choices = unique(watershed_species_risk$huc))
                                          
                                          
                             ), # End sidebarpanel - Species tab 
                             
                             
                             mainPanel(strong("OUTPUT"), # Subheader
                                       # Adding the output from our server
                                       plotlyOutput(outputId = 'species_plot'),
                                       plotlyOutput(outputId = 'watershed_species_plot')
  
                           ) # End main panel - species tab
  
                  ) # End sidebarLayout - species tab
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
  
  ## Creating data set for reactive input for species selection
  species_select <- reactive({
    exceed_longer %>%
      select(species, pesticide, huc, days) %>%
      dplyr::filter(species == input$species_select) %>%
      slice_max(days, n = 5) %>% # keeping the largest values of the counts by lake
      arrange(-days) # arranges selected choices from greatest to least
  }) # End species select reactive
  
  # Creating a plot using our species data
  output$species_plot <- renderPlotly({
    ggplot(data = species_select(),
           aes(y = days, x = reorder(huc, -days), fill = pesticide)) +
      geom_col() +
      labs(y = 'Days of Exceedance', x = "Watershed",
           title = "Greatest Days of Exceedance per Species") +
      coord_flip() +
      theme_minimal()
  }) # End species reactive plot
  
  
  watershed_species_select <- reactive({
    watershed_species_risk %>%
      select(species, pesticide, huc, days) %>%
      dplyr::filter(huc == input$watershed_species_select) %>%
      slice_max(days, n = 15) %>% # keeping the largest values of the counts by day
      arrange(-days) # arranges selected choices from greatest to least
  }) # End species watershed reactive
  
  
  # Creating a watershed plot using our species data
  output$watershed_species_plot <- renderPlotly({
    ggplot(data = watershed_species_select(),
           aes(y = days, x = reorder(species, -days), fill = pesticide)) +
      geom_col(position = "dodge") +
      labs(y = 'Days of Exceedance', x = "Watershed",
           title = "Days of Species Pesticide Exposure Exceedance per Watershed") +
      coord_flip() +
      theme_minimal()
  }) # End watershed reactive plot

  
  #######################################################################################
  
} # end server function 



# Run the application ----
shinyApp(ui = ui, server = server)

