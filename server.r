library(shiny)
library(highcharter)

shinyServer(function(input,output){
  output$distPlot <- renderPlot({
    x <- faithful[,2]
    bins <- seq(min(x),max(x), length.out = (input$bins + 1))
    
    hist(x,breaks = bins, col = 'darkgrey', border = 'white')
  })
})