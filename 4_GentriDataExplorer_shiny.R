#********************************************
#                                           #
#         LOR GGvis Motion-Chart            #
#                                           #
#********************************************

library(googleVis)
library(shiny)
names(LORdataFULL)
LORdata4ggvis <- LORdataFULL
LORdata4ggvis$ZEIT <- as.numeric(as.character(LORdata4ggvis$ZEIT))

GentriMotionChartUI <- shinyUI(pageWithSidebar(
  headerPanel("Gentri-MotionChart"),
  sidebarPanel(
    checkboxGroupInput("AusgewaehlteBezirke","Bezirke:",
                       choices = levels(LORdata4ggvis$BEZ_NAME),
                       selected = "Friedrichshain-Kreuzberg"),
    submitButton(text="Update!"),
    width = 3),
  mainPanel(tabsetPanel(
    tabPanel("Motion Chart", helpText("blabla")),
    tabPanel("Daten", dataTableOutput("LORggvisDaten"))
  ))))
  
  GentriMotionChartSERVER <- shinyServer(function (input, output, session) {
    LORdata4ggvisSUBSET <- reactive({
      b <- subset(LORdata4ggvis, BEZ_NAME %in% input$AusgewaehlteBezirke)
      b <- droplevels(b)
      return(b)
    })
    output$LORggvisDaten <- renderDataTable({
      newData <- LORdata4ggvisSUBSET()
      newData})#options = list(lengthMenu = c(10, 50, 100), pageLength = 50))  
  })
  
  GentriMotionChart <- runApp(
    list(server = GentriMotionChartSERVER, 
         ui     = GentriMotionChartUI))
  GentriMotionChart

#######################


library(googleVis)
library(shiny)
names(LORdataFULL)
LORdata4ggvis <- LORdataFULL
LORdata4ggvis$ZEIT <- as.numeric(as.character(LORdata4ggvis$ZEIT))

GentriMotionChartUI <- shinyUI(navbarPage("GentriData Explorer",
                     tabPanel("Motion Chart",
                              pageWithSidebar(
                                headerPanel("Motion Chart"),
                                sidebarPanel(
                                  checkboxGroupInput("AusgewaehlteBezirke","Bezirke:",
                                                     choices = levels(LORdata4ggvis$BEZ_NAME),
                                                     selected = "Friedrichshain-Kreuzberg"),
                                  submitButton(text="Update!")),
                                mainPanel("LOR Planungsräume von Berlin", htmlOutput("LORggvis")))
                              ),
                     tabPanel("Daten", 
                              pageWithSidebar(
                                headerPanel("Daten"),
                                sidebarPanel(
                                  checkboxGroupInput("VarSelection","Variablen:",
                                                     choices = names(LORdata4ggvis),
                                                     selected = c("ZEIT",
                                                                  "RAUMID_NAME")),
                                submitButton(text="Update!")),
                                mainPanel("Rohdaten", 
                                          dataTableOutput("LORggvisDaten"))
                     )
                     )
))

GentriMotionChartSERVER <- shinyServer(function (input, output, session) {
  LORdata4ggvisSUBSET <- reactive({
    b <- subset(LORdata4ggvis, BEZ_NAME %in% input$AusgewaehlteBezirke)
    b <- droplevels(b)
    return(b)
  })
  output$LORggvis <- renderGvis({
    newData <- LORdata4ggvisSUBSET()
    LORggvis <- gvisMotionChart(newData, 
                                idvar   = "RAUMID_NAME",
                                timevar = "ZEIT",
                                sizevar = "E_E",
                                colorvar= "BEZ_NAME",
                                options=list(width=800, 
                                             height=500))
    return(LORggvis)
  })
  output$LORggvisDaten <- renderDataTable({
    newData <- LORdata4ggvisSUBSET()
    newData[,input$VarSelection, drop = FALSE]},
                                          options = list(lengthMenu = c(5, 10, 50, 100, 500), 
                                                         pageLength = 50))  
})

GentriMotionChart <- runApp(
  list(server = GentriMotionChartSERVER, 
       ui     = GentriMotionChartUI))
GentriMotionChart





  