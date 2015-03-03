library(googleVis)
LORdata4ggvisLOCAL <- read.csv("./LORdata4ggvis.csv",
                               sep=";",
                               dec=".")

shinyUI(navbarPage("GentriData Explorer",
                   tabPanel("Motion Chart",
                            pageWithSidebar(
                              headerPanel("Motion Chart"),
                              sidebarPanel(
                                checkboxGroupInput("AusgewaehlteBezirke","Bezirke:",
                                                   choices = levels(LORdata4ggvisLOCAL$BEZ_NAME),
                                                   selected = "Friedrichshain-Kreuzberg"),
                                submitButton(text="Update!")),
                              mainPanel("LOR Planungsräume von Berlin", htmlOutput("LORggvis")))
                   ),
                   tabPanel("Daten", 
                            pageWithSidebar(
                              headerPanel("Daten"),
                              sidebarPanel(
                                checkboxGroupInput("VarSelection","Variablen:",
                                                   choices = names(LORdata4ggvisLOCAL),
                                                   selected = c("ZEIT",
                                                                "RAUMID_NAME")),
                                submitButton(text="Update!")),
                              mainPanel("Rohdaten", 
                                        dataTableOutput("LORggvisDaten"))
                            )
                   )
))