library(shiny)
library(ggplot2)
library(lubridate)
library(magrittr)
library(formattable)
library(shinydashboard)
library(stringr)

# load dataset
load("../../../data/trPatients.Rdata")
#load("../../../data/trChartLabEvents.Rdata")
load("../../../data/trChartEvents.Rdata")

# Define a server for the Shiny app
shinyServer(function(input, output,session) {
  
  # Filter data based on selections
  output$mainTable <- DT::renderDataTable({
    DT::datatable(trPatients)
  })

  #retrieve all charts and lab values for selected patients
  chartsLabsDf <- reactive({
    rowIndices <- input$mainTable_rows_selected %>% as.numeric
    ids <- trPatients[rowIndices,] %>%
      select(subject_id,hadm_id,daysInHospital)
    trChartEvents %>%
      inner_join(ids)
  })
  
  #number of events recorded for each chart item
  chartLabEventsSummary <- reactive({
      temp<-chartsLabsDf()  %>%
      group_by(itemid,label,subject_id) %>%
      #daysInHospital will be the same per group but
      #we just need to select any of them so we choose max
      summarize(numEventsPerDay=floor(n()/as.numeric(max(daysInHospital)))) %>%
      ungroup() %>%
      group_by(itemid,label) %>%
      summarize(medNumEventsPerDay=median(numEventsPerDay),
                Q5=quantile(numEventsPerDay,probs=0.05),
                Q95=quantile(numEventsPerDay,probs=0.95)) %>%
      ungroup
      
      temp %>% print.data.frame
      
      temp %>% select(itemid,label,medNumEventsPerDay,Q5,Q95) %>%
      arrange(desc(medNumEventsPerDay))
  })
  
  output$selectVars <- renderUI({
    req(input$mainTable_rows_selected)
    
    df <- chartLabEventsSummary()
    labels <- df$label
    numEventsStr <- paste0(df$Q5,"-[",df$medNumEventsPerDay,"]-",df$Q95)     
    varStr <- paste(labels,numEventsStr)
    
    checkboxGroupInput("varCheckboxes","Select Measurements to Plot",
                       choices=varStr)
  })
  
  observeEvent(input$plot,{
    output$mainPlot <- renderPlot({
      checkedVars <- input$varCheckboxes %>%
        str_replace(" [0-9]+-\\[[0-9]+\\]-[0-9]+","")
      df<-chartsLabsDf() %>%
      filter(label %in% checkedVars)
      print(df)
      ggplot(df,aes(x=timeSinceEdReg,y=valuenum,group=subject_id,col=subject_id)) +
        geom_point() +
        geom_line() +
        facet_grid(label ~ .,scale="free_y")
    })
  })
  
  
})

