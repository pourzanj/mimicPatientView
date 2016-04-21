library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(magrittr)
library(formattable)
library(shinydashboard)
library(stringr)

# load dataset
load("data/trPatients.Rdata")

# Define a server for the Shiny app
shinyServer(function(input, output,session) {
  
  # Filter data based on selections
  output$mainTable <- DT::renderDataTable({
    trPatientsFiltered <- trPatients %>%
      filter(input$ageRange[1] <= age & age <= input$ageRange[2]) %>%
      filter(input$hospitalStayRange[1] <= daysInHospital & daysInHospital <= input$hospitalStayRange[2]) %>%
      filter(input$totalRbcRange[1] <= totalRBC & totalRBC <= input$totalRbcRange[2])
    
    DT::datatable(trPatientsFiltered)
  })

  #retrieve all charts and lab values for selected patients
  chartsDf <- reactive({
    rowIndices <- input$mainTable_rows_selected %>% as.numeric
    
    #return NULL if no patients are selected
    if(length(rowIndices)==0)
      return(NULL)
    
    ids <- trPatients[rowIndices,] %>%
      select(subject_id,hadm_id,daysInHospital)
    
    trChartEvents %>% inner_join(ids)
  })
  
  #number of events recorded for each chart item
  chartLabEventsSummary <- reactive({
    df <- chartsDf()
    if(is.null(df))
      return(NULL)
    temp<-df %>%
      group_by(itemid,label,subject_id) %>%
      #daysInHospital will be the same per group but
      #we just need to select any of them so we choose the first with head
      summarize(numEventsPerDay=floor(n()/as.numeric(head(daysInHospital,1)))) %>%
      ungroup() %>%
      group_by(itemid,label) %>%
      summarize(medNumEventsPerDay=median(numEventsPerDay,na.rm=TRUE),
                Q5=quantile(numEventsPerDay,probs=0.05),
                Q95=quantile(numEventsPerDay,probs=0.95)) %>%
      ungroup %>%
      select(itemid,label,medNumEventsPerDay,Q5,Q95) %>%
      arrange(desc(medNumEventsPerDay))
    
    print(temp)
    temp
  })
  
  #this function will only get updated if getEvents gets clicked. if
  #it does get updated then eventTableUi will get updated placing either
  #text or eventTable in the ui file. 
  updateEventTableUi <- eventReactive(input$getEvents,{
    df <- chartLabEventsSummary()
    print("checking for null df")
    if(is.null(df))
      return("Select Patients to View Available Event Logs")
    print("returning output")
    DT::dataTableOutput("eventTable")
  })
  output$eventTableUi <- renderUI({
    updateEventTableUi()
  })
  
  #once a table is placed in the ui from the code directly above,
  #the table will only get updated when we click the getEvents button
  updateEventTable <- eventReactive(input$getEvents,{
    DT::datatable(chartLabEventsSummary())
  })
  output$eventTable <- DT::renderDataTable({
    updateEventTable()
  })
  
  #retrieve all charts and lab values for selected patients
  selectedCharts <- eventReactive(input$plot,{
    rowIndices <- input$eventTable_rows_selected %>% as.numeric
    
    #return NULL if no charts are selected
    if(length(rowIndices)==0)
      return(NULL)
    
    #otherwise figure out the itemids of the chart charts selected and
    #only pick out the the chart events with those itemids from the df
    #of all the chart events and return that
    rowIndices <- input$eventTable_rows_selected %>% as.numeric
    itemdIdsSelected <- (chartLabEventsSummary())[rowIndices,] %>%
      select(itemid)
    
    chartsDf() %>%
      inner_join(itemdIdsSelected)
  })
  
  observeEvent(input$plot,{
    output$mainPlot <- renderPlot({
      
      df <- selectedCharts()
      if(is.null(df))
        return("No Charts Selected. Nothing to Plot.")
      
      ggplot(df,aes(x=timeSinceAdmit,y=valuenum,group=factor(subject_id),color=factor(subject_id))) +
        facet_grid(label ~ .,scale="free_y") +
        #geom_line(data=subset(df,measType != "Transfusion")) +
        #geom_smooth(data=subset(df,measType != "Transfusion")) +
        #geom_point(data=subset(df,measType == "Transfusion"),aes(shape=fluid),size=4) +
        geom_line() +
        geom_smooth() +
        scale_x_datetime(labels=date_format("Day %d \n %H:%M"))
    },height=1000,width=1200)
  })
  
  
})

