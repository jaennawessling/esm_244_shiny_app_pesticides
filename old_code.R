### old tab 3 code

# Tab 3 - Application site type - Sadie ----
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