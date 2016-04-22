library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("MIMIC Patient Viewer"),
    
    # create new for table
    fluidRow(
      DT::dataTableOutput("mainTable")
    ),
    
    #event table
    fluidRow(
      actionButton("getEvents","Retreive Events for Selected Patients")
    ),
    fluidRow(
      uiOutput("eventTableUi")
    ),
    
    sidebarLayout(
      uiOutput("plotOptionsUi"),
      mainPanel(
        plotOutput('mainPlot',width="100%")
      )
    )
    
    
  )
)