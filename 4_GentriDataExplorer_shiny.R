#********************************************
#                                           #
#         LOR GGvis Motion-Chart            #
#                                           #
#********************************************

devtools::install_github('rstudio/shinyapps')
library("shinyapps")
library(googleVis)
library(shiny)

LORdata4ggvis <- LORdataFULL # muss LONG datensatz sein!
LORdata4ggvis$ZEIT <- as.numeric(as.character(LORdata4ggvis$ZEIT))
# Charlottenburg-Wilmersdorf funktioniert nicht aus irgendwelchen Gründen. Ich dachte erst es liegt an der Länge des Names, ist aber nicht so:
levels(LORdata4ggvis$BEZ_NAME)[which(levels(LORdata4ggvis$BEZ_NAME) == "Charlottenburg-Wilmersdorf")] <- "Cburg-Wilmersdorf"
# Charlottenburg funktioniert trotzdem nicht

write.table(LORdata4ggvis, 
            file = "/home/dao/Desktop/MasterArbeit/R_files/ShinyApps/LORdata4ggvis.csv", 
            append = FALSE, quote = TRUE, sep = ";", na = "NA", dec = ".", col.names = TRUE)


GentriMotionChartUI <- 
  shinyUI(navbarPage("GentriData Explorer",
                     tabPanel("Motion Chart",
                              pageWithSidebar(
                                headerPanel("Motion Chart"),
                                sidebarPanel(
                                  checkboxGroupInput("AusgewaehlteBezirke","Bezirke:",
                                                     choices = c("Cburg-Wilmersdorf"  = 	"Cburg-Wilmersdorf",
                                                                 "Friedrichshain-Kreuzberg"  	 = 	"Friedrichshain-Kreuzberg",  
                                                                 "Lichtenberg"               	 = 	"Lichtenberg"             ,  
                                                                 "Marzahn-Hellersdorf"       	 = 	"Marzahn-Hellersdorf"      , 
                                                                 "Mitte"                     	 = 	"Mitte"                    , 
                                                                 "Neukölln"                  	 = 	"Neukölln"                  ,
                                                                 "Pankow"                    	 = 	"Pankow"                    ,
                                                                 "Reinickendorf"             	 = 	"Reinickendorf"             ,
                                                                 "Spandau"                   	 = 	"Spandau"                   ,
                                                                 "Steglitz-Zehlendorf"       	 = 	"Steglitz-Zehlendorf"       ,
                                                                 "Tempelhof-Schöneberg"      	 = 	"Tempelhof-Schöneberg"      ,
                                                                 "Treptow-Köpenick"  	         = 	"Treptow-Köpenick"),
                                                     #levels(LORdata4ggvis$BEZ_NAME),
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

GentriMotionChartSERVER <- 
  shinyServer(function (input, output, session) {
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

setwd("/home/dao/Desktop/MasterArbeit/R_files/ShinyApps/")
LORdata4ggvisLOCAL <- read.csv("./LORdata4ggvis.csv",
                               sep=";",
                               dec=".")
runApp()





  



  