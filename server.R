
# install packages if ----
for (pkg in c('shiny','ggplot2','scales','lubridate','magrittr','formattable','shinydashboard','stringr')){
  if (!require(pkg, character.only=T)){
    install.packages(pkg)
    library(pkg)
  }
}

# load libraries in standard way ----
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(magrittr)
library(formattable)
library(shinydashboard)
library(stringr)

# Load Data Set ------------------------------------------------------
data_dir = ifelse(
  Sys.info()[['nodename']] == "salacia.local",
  "mimicPatientViewData",
  "~/Dropbox/mimicPatientViewData")
load(file.path(data_dir, "trPatients.Rdata"))
load(file.path(data_dir, "trEvents.Rdata"))

# Server Definition -------------------------------------------------
shinyServer(function(input, output,session) {
  
  # Patient Table
  output$mainTable <- DT::renderDataTable({
    DT::datatable(trPatients)
  })

  #retrieves a basic dataframe with the ids and daysInHospital
  #for the patients that have been selected
  selectedPatientIdDf <- reactive({
    rowIndices <- input$mainTable_rows_selected %>% as.numeric
    
    #return NULL if no patients are selected
    if(length(rowIndices)==0)
      return(NULL)
    
    ids <- trPatients[rowIndices,] %>%
      select(subject_id,hadm_id,daysInHospital,hoursToAdmit)
  })
  
  #retrieve all charts and lab values for selected patients
  chartsDf <- reactive({
    ids <- selectedPatientIdDf()
    trEvents %>% inner_join(ids)
  })
  
  #this function summarizes the measurements the selected patients have
  #to display in the events table
  chartLabEventsSummary <- reactive({
    df <- chartsDf()
    if(is.null(df))
      return(NULL)
    
    df %>%
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
  })
  
  #this function will only get updated if getEvents gets clicked. if
  #it does get updated then eventTableUi will get updated placing either
  #text or eventTable in the ui file. We have this because we don't want
  #an empty eventTable to show up if no patients are selected
  updateEventTableUi <- eventReactive(input$getEvents,{
    df <- chartLabEventsSummary()
    
    if(is.null(df))
      return("Select Patients to View Available Event Logs")

    DT::dataTableOutput("eventTable")
  })
  output$eventTableUi <- renderUI({
    updateEventTableUi()
  })
  
  #analogously, we will also only display plotting options whence
  #the eventTable has been shown
  updatePlotOptionsUi <- eventReactive(input$getEvents,{
    df <- chartLabEventsSummary()
    if(is.null(df))
      return()
    
    patientDf <- selectedPatientIdDf()
    earliestEdReg <- -(((patientDf$hoursToAdmit %>% max(na.rm=TRUE))/24) %>% ceiling)
    maxDaysInHospital <- patientDf$daysInHospital %>% max(na.rm=TRUE)
    
    sidebarPanel(
      checkboxGroupInput("selectPlotOptions","Plot Geometry",
                         c("Points","Lines","Smooth","Error Bars"),
                         selected=c("Lines")),
      sliderInput("plotTimeRangeSlider","Time Range to Plot",min=earliestEdReg,max=maxDaysInHospital,value=c(earliestEdReg,maxDaysInHospital)),
      actionButton("plot","Plot"),
      width=2
    )
  })
  output$plotOptionsUi <- renderUI({
    updatePlotOptionsUi()
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
    itemsSelected <- (chartLabEventsSummary())[rowIndices,] %>%
      select(label) %>%
      rbind(data.frame(label="Transfusion"),data.frame(label="Transfer"))
    
    chartsDf() %>%
      inner_join(itemsSelected)
  })
  
  
  #main plot retrieves plotting information then returns ggplot object
  observeEvent(input$plot,{
    output$mainPlot <- renderPlot({
      
      df <- selectedCharts()
      if(is.null(df))
        return("No Charts Selected. Nothing to Plot.")
      
      start <- "2000-01-01 00:00:00" %>% ymd_hms
      dateLimits <- (c(start+input$plotTimeRangeSlider[1]*60*60*24,start+input$plotTimeRangeSlider[2]*60*60*24) %>%
        force_tz(tzone="UTC")) + 60*60*8
  
      idLevels <- factor(df$subject_id) %>% levels
      
      p <- ggplot(df,aes(x=timeSinceAdmit,y=valuenum,group=factor(subject_id),color=factor(subject_id))) +
        facet_grid(label+measType ~ .,scale="free_y") +
        geom_point(data=subset(df,measType=="Transfer"),aes(size=valuenum,color=factor(subject_id))) +
        geom_label_repel(data=subset(df,measType=="Transfer"),aes(label=value,fill=factor(subject_id)),color="white",fontface="bold",size=3) +
        geom_line(data=subset(df,measType=="Chart" & isFactor)) +
        geom_label(data=subset(df,measType=="Chart" & isFactor),aes(label=value,fill=factor(subject_id)),color="white",fontface="bold",size=3) +
        geom_point(data=subset(df,measType == "Transfusion"),aes(size=valuenum,color=factor(subject_id))) +
        geom_label_repel(data=subset(df,measType == "Transfusion"),aes(label=fluid,fill=factor(subject_id)),color="white",fontface="bold",size=3) +
        scale_fill_discrete(breaks=idLevels,limits=idLevels,drop=FALSE) +
        scale_x_datetime(labels=date_format("Day %d \n %H:%M")) +
        coord_cartesian(xlim=dateLimits)
      
      chartPlottingOptionsSelected <- input$selectPlotOptions
      print(chartPlottingOptionsSelected)
      if("Lines" %in% chartPlottingOptionsSelected)
        p <- p + geom_line(data=subset(df,measType == "Chart" & !isFactor))
      if("Points" %in% chartPlottingOptionsSelected)
        p <-p + geom_point(data=subset(df,measType == "Chart" & !isFactor))
      if("Smooth" %in% chartPlottingOptionsSelected & "Error Bars"  %in% chartPlottingOptionsSelected)
        p <-p + geom_smooth(data=subset(df,measType == "Chart" & !isFactor),se=TRUE)
      if("Smooth" %in% chartPlottingOptionsSelected & !("Error Bars"  %in% chartPlottingOptionsSelected))
        p <-p + geom_smooth(data=subset(df,measType == "Chart" & !isFactor),se=FALSE)
      
      return(p)
    },height=1000,width=1200)
  })
  
  
})

