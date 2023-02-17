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

# Reading in our example data (just temporary to practice until we get the real data set)
pesticides <- read_excel(here('Example_Output_DataTable.xlsx')) %>% 
  clean_names() %>% 
  mutate(across(where(is.character), tolower)) # changing the characters to lower case 
# View(pesticides) # can uncomment this if you want to view the temporary data 
# Should I try to remove the numbers and letters before each pesticide name, or is it part of the name? 


my_theme <- bs_theme(
  bootswatch = "minty") 


# Define UI ---- 
ui <- fluidPage(theme = my_theme,
                
                # Application title
                titlePanel("The Pesticide Management Prioritization Module (PMPM)"),
                
                # Adding our tabs panel
                tabsetPanel(
                  
                  # Welcome Tab - Jaenna ----
                  tabPanel("Welcome",
                           
                           # Creating sidebar widget first 
                           sidebarLayout(
                             sidebarPanel("WIDGET",
                                          selectInput(
                                            "select", 
                                            label = h3("Select application site type"), 
                                            choices = c("Nursery", "Almond Tree Orchard", "Squash Farm"), 
                                            selected = 1) # end selectInput
                             ), # end sidebarPanel widgets - Welcome tab
                             
                             # Adding text and ouput to the main panel
                             mainPanel(
                               strong("Purpose"),
                               p("This interactive tool illustrates the daily predicted pesticide concentrations and risk
                     based on toxicity to fish, aquatic invertebrates, aquatic nonvascular plants (algae), 
                     and aquatic vascular plants in the Bay Delta Watershed. "), # End paragraph 1 
                     br(), # Line break
                     strong("Background"),
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
                     and preserve ecosystem health?"), # end paragraph 2
                     # Adding the output from our server (temporary - need to add in the real function later)
                     strong("OUTPUT"), # Subheader
                     "output$value1" # Temporary function
                             ) # End mainPanel - Welcome page
                           ) # end sidebarLayout - Welcome tab 
                  ), # End tabPanel - Welcome Page
                  
                  # Map tab - Kira ----
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
                  
                  # Temporal trends by pesticide tab - Sadie ----
                  tabPanel("Temporal Trends by Crop",
                           sidebarLayout(
                             #dropdown menus
                             sidebarPanel("Pesticides",
                                          #dropdown  menu for pesticide type
                                          selectInput("pesticide_dropdown",
                                                      label = "Select pesticide",
                                                      choices = c("Pesticide 1" = 1, "Pesticide 2" = 2, "Pesticide 3" = 3, "Pesticide 4" = 4, "Pesticide 5" = 5)), #end pesticide dropdown
                                          
                                          #dropdown menu for watershed 
                                          "Watersheds",
                                          selectInput("watershed_dropdown",
                                                      label = "Select watershed",
                                                      choices = c("Watershed 1" = 1, "Watershed 2" = 2, "Watershed 3" = 3)) #end watershed dropdown
                                          ), #end sidebarPanel
                             #display  the graph of temporal trends for the selected pesticide and watershed
                             mainPanel("Graph  of temporal trends by pesticide and watershed",
                                       plotOutput(outputId = 'pesticide_plot') #tell the app where to put the graph
                             ) #end mainPanel
                           ) #end sidebarLayout        
                  ), #end tabPanel - temporal trends by crop
                  
  
                  # Animals tab - Jaenna ----
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

# Define server ----
server <- function(input, output) {
  
  # Tab 1 - Welcome output - Jaenna ----
  # Just using sample output from the widget gallery website for now 
  output$value1 <- renderPrint({ input$select })
  
  # Tab 2 - Map output - Kira ----
  output$range <- renderPrint({ input$tox_yr_slider }) #PLACEHOLDER - will change with graph 
  
  # Tab 3 - Temporal trends for pesticide output - Sadie ----
  #reactive data frame to select pesticide and watershed
  pesticide_watershed_df <- reactive ({
    pesticides %>% 
      filter(pesticide == input$pesticide_dropdown) %>% 
      filter(watershed == input$watershed_dropdown)
     # group_by() %>% 
     # summarize()
  })
  
  #render plot of pesticide for a watershed
  output$pesticide_plot <- renderPlot({
    ggplot(data = pesticides_watershed_df(),
           aes(x = year, y = pesticide_concentration)) +
      geom_col() 
  })
  
  # Tab 4 - Animals output - Jaenna ----
  # Just using sample output from the widget gallery website for now 
  output$value2 <- renderPrint({ input$select })
  
} # end server function 



# Run the application ----
shinyApp(ui = ui, server = server)

