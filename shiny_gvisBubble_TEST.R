library(googleVis)
library(shiny)
runApp(
  list(ui = fluidPage(
    tabsetPanel(
      tabPanel("Plot", htmlOutput("MieteGGVIS"), style = "overflow:hidden;"),
      tabPanel("Summary"),
      tabPanel("Table")
    )
  )
  , server = function(input, output, session) {
    output$MieteGGVIS <- renderGvis({
     gvisBubbleChart(ExDF, idvar="RAUMID_NAME", 
                                        xvar="MIETE_chgr", yvar="HK_Turkchgr",
                                        colorvar="BEZ_NAME", sizevar="E_E.2013",
                                        options=list(width=1200, height=700)
      )  
    }) 
  })
)

runApp(
shinyUI(pageWithSidebar(
  headerPanel("Miete und Änderung des Türkischen Einwohneranteils"),
    sidebarPanel(
    checkboxGroupInput("BEZ_NAME","Bezirk",
                       c(
      "Charlottenburg-Wilmersdorf"="Charlottenburg-Wilmersdorf"
      "Friedrichshain-Kreuzberg"="Friedrichshain-Kreuzberg",
      "Lichtenberg"="Lichtenberg",
      "Marzahn-Hellersdorf"="Marzahn-Hellersdorf", 
      "Mitte"="Mitte",
      "Neukölln"="Neukölln",
      "Pankow"="Pankow",
      "Reinickendorf"="Reinickendorf",
      "Spandau"="Spandau",
      "Steglitz-Zehlendorf"="Steglitz-Zehlendorf",
      "Tempelhof-Schöneberg"="Tempelhof-Schöneberg",
      "Treptow-Köpenick"="Treptow-Köpenick")),
    submitButton(text="Fertig")),
  mainPanel(
    tabsetPanel(
      tabsetPanel(
        tabPanel("Plot", htmlOutput("MieteGGVIS"), style = "overflow:hidden;"),
        tabPanel("Summary"),
        tabPanel("Table")
    )
  )
))
,
shinyServer(function (input, output) {
  data.bubble <- reactive({
    b <- subset(ExDF, Bezirk %in% input$region)
    b <- droplevels(b)
    return(b)
  })
    output$MieteGGVIS <- renderGvis({
      gvisBubbleChart(data.bubble, idvar="RAUMID_NAME", 
                      xvar="MIETE_chgr", yvar="HK_Turkchgr",
                      colorvar="BEZ_NAME", sizevar="E_E.2013",
                      options=list(width=1200, height=700)
      )  
    }) 
  })
)