library(shiny)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("MIMIC Patient Viewer"),
    
    # 1. Patient Table -------------------------------------------------
    fluidRow(
      DT::dataTableOutput("mainTable")
    ),
    fluidRow(
      #once patients are selected this action button is what triggers 
      #the program to retrieve their recorded events and plot it in the
      #events table.
      actionButton("getEvents","Retreive Events for Selected Patients")
    ),
    
    # 2. Event Table ---------------------------------------------------
    fluidRow(
      #here we use uiOutput which allows for output formats to change dyanmically.
      #So we could output text, a table, an image or whatever we need to.
      #If no patients are selected there should be events and hence no events
      #table to show. Otherwise we should output a table.
      uiOutput("eventTableUi")
    ),
    
    # 3. Main Plotting Area --------------------------------------------
    sidebarLayout(
      # If no patients and events are selected, we don't want anything output here.
      #Otherwise we do want the plotting options shown.
      uiOutput("plotOptionsUi"),
      
      #this is where the main plot of the program actually goes.
      mainPanel(
        plotOutput('mainPlot',width="100%")
      )
    )
    
    
  )
)