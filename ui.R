library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("MIMIC Patient Viewer"),
    
    #filter patients table
    fluidRow(
      column(4,
             selectInput("mortality",
                         "Mortality Status:",
                         c("All","Died","Survived")
                         )
      )
    ),
    
    # create new for for table
    fluidRow(
      DT::dataTableOutput("mainTable")
    ),

    sidebarLayout(
      # get patient chart vars
      sidebarPanel(
        uiOutput("selectVars",style = "overflow-y:scroll; max-height: 200px"),
        actionButton("plot","Plot"),
        width=2
      ),
      mainPanel(
        #verbatimTextOutput("mainPlot")
        plotOutput('mainPlot')
      )
    )
    

  )
)