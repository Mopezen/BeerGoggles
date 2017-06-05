library(shiny)
library(highcharter)

shinyUI(fluidPage(
  h1("BeerSights!",align="center"),
  fluidRow(
    column(12,highchartOutput("beerChart"))
  ),
  hr(),
  fluidRow(
    column(4,
      wellPanel(
        h4("Controls"),
        sliderInput('maxPages', 'Max Number of Pages',
                    min = 1, max = 10, value = 1, step = 1),
        checkboxInput('ocb','Show Only Ontario Craft Beers?')
      )
    ),
    column(8, align = "center",
           actionButton('generateChart','Generate Chart',style='width: 90%; height: 80%'))
  )
))