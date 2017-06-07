#Check and install missing packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","highcharter","jsonlite","dplyr")

shinyServer(function(input,output){
  output$beerChart <- renderHighchart({
      if (input$generateChart == 0)
        return()
      
      #Create our JS function to return point values
      pointClickedFunction <- JS("function(event){Shiny.onInputChange('pointClicked',
                         [this.label,this.primary_category,this.secondary_category,
                         this.varietal,this.tertiary_category,this.style,this.img]);}")
      
      isolate({
        #Create our progess object
        progress <- shiny::Progress$new()
        on.exit(progress$close()) #Close progress on exit
        
        #List of beer attributes (Name-Character, $/L of Alcohol-Numeric & Amount of Alcohol-Numeric)
        beerList <- list()
        
        curPage <- 1 #Current Page of the API Beers
        totalPages <- 1 #Total number of pages used to update progress
        currentVectorLoc <- 1 #The location of placing values in our attribute vector
        maxPages <- input$maxPages
        ocb <- ""
        
        if(input$ocb){
          ocb <- "&where=is_ocb"
        }
      
        progress$set(message="Gathering beers from API", value = 0)
        
        #Loop while we aren't on the last page of the API 
        repeat{
          if(curPage != 1){
            progress$inc(1/totalPages, detail =paste0("Gathering beers from Page ",curPage))
          }
          
          #Get pages from the LCBO API & the number of total pages if we're on the first loop
          beerDF <- fromJSON(paste0("http://lcboapi.com/products?where_not=is_dead,is_discontinued",ocb,"&per_page=100&page=",curPage))
          
          if(beerDF$pager$is_first_page == TRUE){
            totalPages <- beerDF$pager$total_pages
            
            #Preallocate our vector space for our attribute vectors
            recordCount <- beerDF$pager$total_record_count #Could run an equation to calculate ~ the actual amount of records to be gather
            beerList$name <- character(recordCount)
            beerList$priceperlitre <- numeric(recordCount)
            beerList$alcoholamount <- numeric(recordCount)
            beerList$img <- character(recordCount)
            beerList$img_large <- character(recordCount)
            
            #Description of beer
            beerList$primary_category <- character(recordCount)
            beerList$secondary_category <- character(recordCount)
            beerList$varietal <- character(recordCount)
            beerList$tertiary_category <- character(recordCount)
            beerList$style <- character(recordCount)
          }
          
          #Subset the DF to only select the columns we care about
          beerDF <- select(beerDF$result,name,alcohol_content,price_per_liter_of_alcohol_in_cents,image_thumb_url,image_url,
                           primary_category,secondary_category,varietal,tertiary_category,style)
          
          #Copy the current page into our allocated vectors
          beerList$name[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$name
          beerList$priceperlitre[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$price_per_liter_of_alcohol_in_cents/100
          beerList$alcoholamount[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)]<- beerDF$alcohol_content
          beerList$img[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$image_thumb_url
          beerList$img_large[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$image_url
          beerList$primary_category[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$primary_category
          beerList$secondary_category[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$secondary_category
          beerList$varietal[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$varietal
          beerList$tertiary_category[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$tertiary_category
          beerList$style[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$style
          
          
          if(curPage == totalPages | curPage > maxPages){
            break
          }
          
          currentVectorLoc <- currentVectorLoc + length(beerDF$name)
          curPage <- curPage + 1
        }
        
        #Update progress
        progress$set(value = 1,message = "Creating Graph", detail = "")
        
        #Reduce vector size to allocated amount
        currentVectorLoc <- currentVectorLoc - 1 #Move inserted number back one
        beerList$name <- beerList$name[1:currentVectorLoc - 1]
        beerList$priceperlitre <- beerList$priceperlitre[1:currentVectorLoc - 1]
        beerList$alcoholamount <- beerList$alcoholamount[1:currentVectorLoc - 1]
        beerList$img <- beerList$img[1:currentVectorLoc - 1]
        beerList$img_large <- beerList$img_large[1:currentVectorLoc - 1]
        beerList$primary_category <- beerList$primary_category[1:currentVectorLoc - 1]
        beerList$secondary_category <- beerList$secondary_category[1:currentVectorLoc - 1]
        beerList$varietal <- beerList$varietal[1:currentVectorLoc - 1]
        beerList$tertiary_category <- beerList$tertiary_category[1:currentVectorLoc - 1]
        beerList$style <- beerList$style[1:currentVectorLoc - 1] 
        
        #Replace NAs with N/As
        beerList$name[is.na(beerList$name)] <- "N/A" #Should never be NA but just incase
        beerList$priceperlitre[is.na(beerList$priceperlitre)] <- 0
        beerList$alcoholamount[is.na(beerList$alcoholamount)] <- 0
        beerList$img[is.na(beerList$img)] <- "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="
        beerList$img_large[is.na(beerList$img_large)] <- "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="
        beerList$primary_category[is.na(beerList$primary_category)] <- "N/A"
        beerList$secondary_category[is.na(beerList$secondary_category)] <- "N/A"
        beerList$varietal[is.na(beerList$varietal)] <- "N/A"
        beerList$tertiary_category[is.na(beerList$tertiary_category)] <- "N/A" 
        beerList$style[is.na(beerList$style)] <- "N/A"
        
        #Create a scatter plot graph displaying the price alcoholic content of drinks per litre versus their total content
        hc <- highchart() %>%
          hc_title(text = "LCBO Beers By Price Per Litre of Alcohol & Total Alcohol Content") %>%
          hc_chart(type="scatter", zoomType = "xy") %>%
          hc_tooltip(useHTML = TRUE,
                     headerFormat = "",
                     pointFormat = paste('<strong>{point.label}</strong><img src="{point.img}" style="height:60px; width: auto;"></br>
                                         <p>Price Per Litre of Alcohol: ${point.x} </br>
                                         Total Alcohol Volume: {point.y}mL
                                         </p>')
          ) %>%
          hc_xAxis(title = list(text = "Dollars Per Litre of Alcohol")) %>%
          hc_yAxis(title = list(text = "Total Alcohol Content")) %>%
          hc_plotOptions(scatter = list(tooltip = list(
                                          hideDelay = 1000
                                        ),
                                        allowPointSelect = TRUE,
                                        point = list(
                                          events = list(select = pointClickedFunction)
                                        )
                                    )
          ) %>%
          hc_add_series_scatter(x = beerList$priceperlitre, y = beerList$alcoholamount, 
                                label = beerList$name, img = beerList$img, img_large = beerList$img_large,
                                primary_category = beerList$primary_category, secondary_category = beerList$secondary_category,
                                varietal = beerList$varietal, tertiary_category = beerList$tertiary_category,
                                style = beerList$style)
        
        return(hc)
      })
    })
  
  #Reactive variables for the information panel
  makeReactiveBinding("beerInfoName")
  makeReactiveBinding("beerInfoPrimaryCategory")
  makeReactiveBinding("beerInfoSecondaryCategory")
  makeReactiveBinding("beerInfoVarietal")
  makeReactiveBinding("beerInfoTertiaryCategory")
  makeReactiveBinding("beerInfoStyle")
  makeReactiveBinding("beerInfoPicture")
  
  observeEvent(input$pointClicked,{
    beerInfoName <<- input$pointClicked[1]
    beerInfoPrimaryCategory <<- input$pointClicked[2]
    beerInfoSecondaryCategory <<- input$pointClicked[3]
    beerInfoVarietal <<- input$pointClicked[4]
    beerInfoTertiaryCategory <<- input$pointClicked[5]
    beerInfoStyle <<- input$pointClicked[6]
    beerInfoPicture <<- input$pointClicked[7]
  })
  
  output$beerName <- renderText({
    beerInfoName
  })
  
  output$beerPrimaryCategory <- renderText({
    beerInfoPrimaryCategory
  })
  
  output$beerSecondaryCategory <- renderText({
    beerInfoSecondaryCategory
  })
  
  output$beervarietal <- renderText({
    beerInfoVarietal
  })
  
  output$beerTertiaryCategory <- renderText({
    beerInfoTertiaryCategory
  })
  
  output$beerStyle <- renderText({
    beerInfoStyle
  })
  
  output$beerImage <- renderText({
    c('<img src="',beerInfoPicture,'">')
  })
})