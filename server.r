#Check and install missing packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","highcharter","jsonlite","dplyr")

shinyServer(function(input,output){
  output$beerChart <- renderHighchart({
      if (input$generateChart == 0)
        return()

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
            recordCount <- beerDF$pager$total_record_count
            beerList$name <- character(recordCount)
            beerList$priceperlitre <- numeric(recordCount)
            beerList$alcoholamount <- numeric(recordCount)
            beerList$img <- character(recordCount)
            beerList$img_large <- character(recordCount)
          }
          
          #Subset the DF to only select the columns we care about
          beerDF <- select(beerDF$result,name,alcohol_content,price_per_liter_of_alcohol_in_cents,image_thumb_url,image_url)
          
          #Copy the current page into our allocated vectors
          beerList$name[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$name
          beerList$priceperlitre[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$price_per_liter_of_alcohol_in_cents/100
          beerList$alcoholamount[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)]<- beerDF$alcohol_content
          beerList$img[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$image_thumb_url
          beerList$img_large[currentVectorLoc:(currentVectorLoc +length(beerDF$name) - 1)] <- beerDF$image_url
          
          currentVectorLoc <- currentVectorLoc + length(beerDF$name)
          
          if(curPage == totalPages | curPage > maxPages){
            break
          }
          curPage <- curPage + 1
        }
        
        #Update progress
        progress$set(value = 1,message = "Creating Graph", detail = "")
        
        beerList$name <- beerList$name[beerList$name != ""]
        beerList$priceperlitre <- beerList$priceperlitre[beerList$priceperlitre != 0]
        beerList$alcoholamount <- beerList$alcoholamount[beerList$alcoholamount != 0]
        beerList$img <- beerList$img[beerList$img != ""]
        beerList$img_large <- beerList$img_large[beerList$img_large != ""]
        
        
        #Create a scatter plot graph displaying the price alcoholic content of drinks per litre versus their total content
        hc <- highchart() %>%
          hc_title(text = "LCBO Beers By Price Per Litre of Alcohol & Total Alcohol Content") %>%
          hc_chart(type="scatter", zoomType = "xy") %>%
          hc_tooltip(useHTML = TRUE,
                     headerFormat = "",
                     pointFormat = paste('<a href="{point.img_large}"><strong>{point.label}</strong><img src="{point.img}" style="height:60px; width: auto;"></a></br>
                                         <p>Price Per Litre of Alcohol: ${point.x} </br>
                                         Total Alcohol Volume: {point.y}mL
                                         </p>')
          ) %>%
          hc_xAxis(title = list(text = "Dollars Per Litre of Alcohol")) %>%
          hc_yAxis(title = list(text = "Total Alcohol Content")) %>%
          hc_plotOptions(scatter = list(tooltip = list(
                                          hideDelay = 1000
                                        )
                                    )
          ) %>%
          hc_add_series_scatter(x = beerList$priceperlitre, y = beerList$alcoholamount, label = beerList$name, img = beerList$img, img_large = beerList$img_large)
        
        return(hc)
      })
    })
})