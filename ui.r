#Check and install missing packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","highcharter")

shinyUI(fluidPage(
  h1("BeerSights!",align="center"),
  fluidRow(
    column(12,highchartOutput("beerChart"))
  ),
  hr(),
  fluidRow(
    column(4,
      wellPanel(
        strong(h3("Controls")),
        sliderInput('maxPages', 'Max Number of Pages',
                    min = 1, max = 10, value = 1, step = 1),
        checkboxInput('ocb','Show Only Ontario Craft Beers?')
      )
    ),
    column(8, align = "center",
           fluidRow(
            column(12,align = "center",
              actionButton('generateChart','Generate Chart',style='width: 90%; height: 80%')
            )
           ),
           fluidRow(
             br(),
             tabsetPanel(
               tabPanel("Beer Information",
                 column(2,
                        htmlOutput("beerImage")
                 ),
                 column(5,align = "center",
                        strong(h4("Name")), 
                        textOutput('beerName'),
                        strong(h4("Primary Category")),
                        textOutput('beerPrimaryCategory'),
                        strong(h4("Secondary Category")),
                        textOutput('beerSecondaryCategory')
                 ),column(5, align = "center",
                        strong(h4("Varietal")),
                        textOutput('beervarietal'),
                        strong(h4("Tertiary Category")),
                        textOutput('beerTertiaryCategory'),
                        strong(h4("Style")),
                        textOutput('beerStyle')
                 )
               )
             )
         )
  )
)))