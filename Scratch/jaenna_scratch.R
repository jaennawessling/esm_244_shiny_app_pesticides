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
                  
                  
                  # Tab 1 - Welcome Tab - Jaenna ----
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
                             column(
                               br(),
                               tags$img(src="watershed.jpg",width="200px",height="260px", align = "justify"),
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
                     and preserve ecosystem health?"), # end paragraph 2
                                      
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
  
  # Top of the page - Image output
  output$crissy_field <- renderImage({
    
    list(src = "www/crissy_field_3.jpg",
         width = "100%",
         height = 400)
    
  }, deleteFile = F) # end renderImage
  
  # Tab 2 - Application Site Type - Jaenna ----
  # Just using sample output from the widget gallery website for now 
  output$value1 <- renderPrint({ input$select })
  
  
  # Tab 4 - Animals output - Jaenna ----
  # Just using sample output from the widget gallery website for now 
  output$value2 <- renderPrint({ input$select })
  
} # end server function 



# Run the application ----
shinyApp(ui = ui, server = server)

