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
    column(5,
      wellPanel(
        strong(h3("Variable Controls")),
        textInput("searchInput","Graph by a search term",placeholder = "i.e. Cider or Lager..."),
        sliderInput('maxPages', 'Max Number of Pages',
                    min = 1, max = 10, value = 1, step = 1),
        radioButtons('ocb','Ontario Craft Beers', c(
          "Include" = "nopref",
          "Show Only" = "only",
          "Exclude" = "exec"
        ),inline = TRUE),
        radioButtons('vqa','Vintners Quality Alliance Wines', c(
          "Include" = "nopref",
          "Show Only" = "only",
          "Exclude" = "exec"
        ),inline = TRUE),
        radioButtons('kosher','Kosher Product', c(
          "Include" = "nopref",
          "Show Only" = "only",
          "Exclude" = "exec"
        ),inline = TRUE),
        radioButtons('seasonal','Seasonal Product', c(
          "Include" = "nopref",
          "Show Only" = "only",
          "Exclude" = "exec"
        ),inline = TRUE),
        radioButtons('value','Products with Value Added Items', c(
          "Include" = "nopref",
          "Show Only" = "only",
          "Exclude" = "exec"
        ),inline = TRUE),
        radioButtons('miles','Products with Bonus Reward Miles', c(
          "Include" = "nopref",
          "Show Only" = "only",
          "Exclude" = "exec"
        ),inline = TRUE)
      )
    ),
    column(7, align = "center",
           fluidRow(
            column(12,align = "center",
              actionButton('generateChart','Generate Chart',style='width: 90%; height: 80%')
            )
           ),
           fluidRow(
             br(),
             tabsetPanel(
               tabPanel("Drink Information",
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
  ),
  fluidRow(column(12, align = "center",
    em(strong(h6("Meshach Jones 2017 ",style='color: #a6a6a6; font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;')))
    ) 
  )
))