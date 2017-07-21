#Check and install missing packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","highcharter","leaflet","shinycssloaders","shinyjs","miniUI")

shinyUI(miniPage(
  useShinyjs(),
  tags$script('
    $(document).ready(function () {
      navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
      function onSuccess (position) {
        setTimeout(function () {
          var coords = position.coords;
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
        }, 1100)
      }

      function onError (err) {
        Shiny.onInputChange("geolocation", false);
      }
    });
  '),
  miniTitleBar("BeerSights!"),
  miniContentPanel(
    padding = 0,
    scrollable = FALSE,
    fillRow(
      fillCol(
        highchartOutput("beerChart",height = "100%")
      )
    )
  ),
  miniContentPanel(
    padding = 5,
    scrollable = FALSE,
    fillRow(
      flex = c(2,6),
      fillCol(
        width = "100%",
        miniContentPanel(
          scrollable = TRUE,
          padding = 0,
          wellPanel(  
            strong(h3("Variable Controls")),
            textInput("searchInput","Graph by a search term",placeholder = "i.e. Cider or Lager..."),
            sliderInput('maxPages', 'Max Number of Pages',
                        min = 1, max = 10, value = 1, step = 1
            ),
            radioButtons(
              'ocb','Ontario Craft Beers', 
              c("Include" = "nopref","Show Only" = "only","Exclude" = "exec"),
              inline = TRUE
            ),
            radioButtons(
              'vqa','Vintners Quality Alliance Wines', 
              c("Include" = "nopref","Show Only" = "only","Exclude" = "exec"),
              inline = TRUE
            ),
            radioButtons(
              'kosher','Kosher Product', 
              c("Include" = "nopref","Show Only" = "only","Exclude" = "exec"),
              inline = TRUE
            ),
            radioButtons(
              'seasonal','Seasonal Product',
              c("Include" = "nopref","Show Only" = "only","Exclude" = "exec"),
              inline = TRUE
            ),
            radioButtons(
              'value','Products with Value Added Items', 
              c("Include" = "nopref","Show Only" = "only","Exclude" = "exec"),
              inline = TRUE
            ),
            radioButtons(
              'miles','Products with Bonus Reward Miles', 
              c("Include" = "nopref","Show Only" = "only","Exclude" = "exec"),
              inline = TRUE
            )
          )
        )
      ),
      fillCol(
        flex = c(1,8), width = "100%",
        fillRow(
          fillCol(
            actionButton('generateChart','Generate Chart',width = "100%")
          )
        ),
        fillRow(
          miniPage(
            miniTabstripPanel(
              miniTabPanel(
                "Drink Information",
                icon = icon("info-circle"),
                miniContentPanel(padding = 10,
                  scrollable = TRUE,
                  fillRow(
                    flex =c(2,3,3),
                    fillCol(
                      withSpinner(htmlOutput("beerImage"))
                    ),
                    fillCol(
                      strong(h4("Name")),
                      textOutput('beerName'),
                      strong(h4("Primary Category")),
                      textOutput('beerPrimaryCategory'),
                      strong(h4("Secondary Category")),
                      textOutput('beerSecondaryCategory')
                    ),
                    fillCol(
                      strong(h4("Varietal")),
                      textOutput('beervarietal'),
                      strong(h4("Tertiary Category")),
                      textOutput('beerTertiaryCategory'),
                      strong(h4("Style")),
                      textOutput('beerStyle')
                    )
                  )
                )
              ),
              miniTabPanel(
                "Drink Map",
                icon = icon("map"),
                miniContentPanel(
                  scrollable = TRUE,
                  fillRow(
                    flex = c(6,2),
                    fillCol(
                      withSpinner(leafletOutput("lcboLocations"))
                    )
                    # fillCol(
                    #   wellPanel(
                    #     style = "height = 100%; overflow:scroll; overflow-y: hidden",
                    #     htmlOutput("mapTable")
                    #   )
                    # )
                  ),
                  br()
                )
              )
            )
          )
        )
      )
    )
  )
  # h1("BeerSights!",align="center"),
  # fillRow(height = "50%",
  #   fillCol(
  #     miniContentPanel(
  #       padding = 15,
  #       scrollable = FALSE,
  #       highchartOutput("beerChart",height = "100%")
  #     )
  #   )
  # ),
  # fillRow(flex = c(2,6), height = "50%", style = "overflow:scroll; overflow-y: hidden",
  #   fillCol(width = "95%",
  #         wellPanel(  
  #           strong(h3("Variable Controls")),
  #           textInput("searchInput","Graph by a search term",placeholder = "i.e. Cider or Lager..."),
  #           sliderInput('maxPages', 'Max Number of Pages',
  #                       min = 1, max = 10, value = 1, step = 1),
  #           radioButtons('ocb','Ontario Craft Beers', c(
  #             "Include" = "nopref",
  #             "Show Only" = "only",
  #             "Exclude" = "exec"
  #           ),inline = TRUE),
  #           radioButtons('vqa','Vintners Quality Alliance Wines', c(
  #             "Include" = "nopref",
  #             "Show Only" = "only",
  #             "Exclude" = "exec"
  #           ),inline = TRUE),
  #           radioButtons('kosher','Kosher Product', c(
  #             "Include" = "nopref",
  #             "Show Only" = "only",
  #             "Exclude" = "exec"
  #           ),inline = TRUE),
  #           radioButtons('seasonal','Seasonal Product', c(
  #             "Include" = "nopref",
  #             "Show Only" = "only",
  #             "Exclude" = "exec"
  #           ),inline = TRUE),
  #           radioButtons('value','Products with Value Added Items', c(
  #             "Include" = "nopref",
  #             "Show Only" = "only",
  #             "Exclude" = "exec"
  #           ),inline = TRUE),
  #           radioButtons('miles','Products with Bonus Reward Miles', c(
  #             "Include" = "nopref",
  #             "Show Only" = "only",
  #             "Exclude" = "exec"
  #           ),inline = TRUE)
  #         )
  #   ),
  #   fillCol(flex = c(1,8), width = "95%",
  #           fillRow(fillCol(
  #             actionButton('generateChart','Generate Chart',style='width: 100%; height: 80%')
  #           )
  #          ),
  #          fillRow(
  #            miniPage(
  #              miniTabstripPanel(
  #                miniTabPanel(
  #                  miniContentPanel(
  #                    fillCol(
  #                           withSpinner(htmlOutput("beerImage"))
  #                    ),
  #                    fillCol(strong(h4("Name")),
  #                           textOutput('beerName'),
  #                           strong(h4("Primary Category")),
  #                           textOutput('beerPrimaryCategory'),
  #                           strong(h4("Secondary Category")),
  #                           textOutput('beerSecondaryCategory')
  #                    ),fillCol(strong(h4("Varietal")),
  #                           textOutput('beervarietal'),
  #                           strong(h4("Tertiary Category")),
  #                           textOutput('beerTertiaryCategory'),
  #                           strong(h4("Style")),
  #                           textOutput('beerStyle')
  #                    )
  #                  )
  #                )
  #              )
  #            )
  #            # tabsetPanel(
  #            #   tabPanel("Drink Information", style='height:40vh',
  #            #     fillRow(flex = c(2,3,3),
  #            #       fillCol(
  #            #              withSpinner(htmlOutput("beerImage"))
  #            #       ),
  #            #       fillCol(strong(h4("Name")),
  #            #              textOutput('beerName'),
  #            #              strong(h4("Primary Category")),
  #            #              textOutput('beerPrimaryCategory'),
  #            #              strong(h4("Secondary Category")),
  #            #              textOutput('beerSecondaryCategory')
  #            #       ),fillCol(strong(h4("Varietal")),
  #            #              textOutput('beervarietal'),
  #            #              strong(h4("Tertiary Category")),
  #            #              textOutput('beerTertiaryCategory'),
  #            #              strong(h4("Style")),
  #            #              textOutput('beerStyle')
  #            #       )
  #            #     )
  #            #   ),
  #            #   tabPanel("Drink Location", style='height:40vh',
  #            #    fillRow(
  #            #      fillCol(
  #            #             withSpinner(leafletOutput("lcboLocations"))
  #            #      )
  #            #       fillCol(wellPanel(style = "height = 100%; overflow:scroll; overflow-y: hidden",
  #            #                          htmlOutput("mapTable")))
  #            #     )
  #            #   )
  #            # )
  #        )
  #        
  #   )
  # )
  # fluidRow(column(12, align = "center",
  #   em(strong(h6("Meshach Jones 2017 ",style='color: #a6a6a6; font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;')))
  #   )
  # )
  )
)