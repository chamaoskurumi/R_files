shinyServer(function (input, output, session) {
  LORdata4ggvisLOCAL <- read.csv("./LORdata4ggvis.csv",
                            sep=";",
                            dec=".")
  LORdata4ggvisSUBSET <- reactive({
    b <- subset(LORdata4ggvisLOCAL, BEZ_NAME %in% input$AusgewaehlteBezirke)
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
