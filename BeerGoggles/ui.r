library(shiny)
library(highcharter)

shinyUI(fluidPage(
  titlePanel("Beer Goggles!"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("BeerNames")
    ),
    mainPanel(
      highchartOutput("beerChart")
    )
  )
))