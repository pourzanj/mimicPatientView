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
      column(2,
             selectInput("mortality",
                         "Mortality Status:",
                         c("All","Died","Survived")
             )
      ),
      column(2,
             sliderInput("ageRange", "Age Range:",
                         min = 0, max = 400, value = c(0,400))
      ),
      column(2,
             sliderInput("hospitalStayRange", "Hospital Stay Range:",
                         min = 0, max = 200, value = c(0,200))
      ),
      column(2,
             sliderInput("totalRbcRange", "Total RBC Range:",
                         min = 0, max = 20000, value = c(0,20000))
      )
      
    ),
    
    # create new for for table
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
      # get patient chart vars
      sidebarPanel(
        checkboxGroupInput("selectVars","Plot Geometry",
                           c("Points","Lines","Smooth","Error Bars"),
                           selected=c("Points","Lines","Smooth","Error Bars")),
                           #style = "overflow-y:scroll; max-height: 200px"),
        actionButton("plot","Plot"),
        width=2
      ),
      mainPanel(
        #verbatimTextOutput("mainPlot")
        plotOutput('mainPlot',width="100%")
      )
    )
    
    
  )
)