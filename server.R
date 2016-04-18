
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
load("data/trEvents.Rdata")

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
  chartsLabsDf <- reactive({
    rowIndices <- input$mainTable_rows_selected %>% as.numeric
    ids <- trPatients[rowIndices,] %>%
      select(subject_id,hadm_id,daysInHospital)
    trEvents %>%
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
      summarize(medNumEventsPerDay=median(numEventsPerDay,na.rm=TRUE),
                Q5=quantile(numEventsPerDay,probs=0.05),
                Q95=quantile(numEventsPerDay,probs=0.95)) %>%
      ungroup %>%
      select(itemid,label,medNumEventsPerDay,Q5,Q95) %>%
      arrange(desc(medNumEventsPerDay))
    
    print(temp)
    temp
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
        str_replace(" [0-9]+[.]{0,1}[0-9]{0,1}-\\[[0-9]+[.]{0,1}[0-9]{0,1}\\]-[0-9]+[.]{0,1}[0-9]{0,1}","")
      print(checkedVars)
      
      df<-chartsLabsDf() %>%
        filter(label %in% c(checkedVars,"Transfusion"))
      print(df)
      
      ggplot(df,aes(x=timeSinceAdmit,y=valuenum,group=factor(subject_id),color=factor(subject_id))) +
        facet_grid(label ~ .,scale="free_y") +
        geom_line(data=subset(df,measType != "Transfusion")) +
        geom_smooth(data=subset(df,measType != "Transfusion")) +
        geom_point(data=subset(df,measType == "Transfusion"),aes(shape=fluid),size=4) +
        scale_x_datetime(labels=date_format("Day %d \n %H:%M"))
    },height=1000,width=1200)
  })
  
  
})

