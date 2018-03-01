data("Europe")
height <- 550L
width <- 550L





#setwd("D:/Moje dokumenty/Praca_magisterska/MagisterkaNetCDF/shiny-master/")

messageData<-data.frame(1,1)
names(messageData)<-c("from","message")
messageData <- messageData[-1,]

messageData[nrow(messageData) + 1,] = c("Support","v2")


#m <- 1
shinyServer(function(input, output, session) {
  
  
output$loadtext <-   renderText({
  

  
  if(load_text==TRUE){
    paste("Trwa przeliczanie nowych danych, proszę czekać.")
  }
    
  })

  dinput <- reactiveVal(0)
  

  
    output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    
    
    
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  
####TEMPERATURA
   observeEvent(input$go,{output$UIPLOT_TMP <- renderUI({ plotOutput("TMP", height = 550, width = 550)})})
 
    observeEvent(input$go, {
      
      output[["TMP"]] <- renderPlot(execOnResize = T, res = 60,{
        dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
        create_raster(minus=-273.15,time = input$bins,rodzaj = "TMP",start = -30,stop = 40,by = 2,togeneralize=T,pinput = dinput, 
                      colory=c("#330033","#330066","#330099","#3300CC","#3366CC","#336666",
                               "#33FF66","#33FF00", "#FFFF66", "#FFFF33", "#FFFF00",
                               "#FF9933","#FF9900","#FF6600","#FF3300","#FF0000","#660000"))
      })
      


      })

  output$legend_TMP <- renderImage({list(src = "legend_SBCAPE.jpg",filetype = "image/jpeg",alt = "This is a chainring")}, deleteFile = FALSE)
    
############################################################################################
  
  #################TMAX###############################
  observeEvent(input$go,{output$UIPLOT_TMAX <- renderUI({ plotOutput("TMAX", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["TMAX"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=-273.15,time = input$bins,rodzaj = "TMAX",start = -30,stop = 40,by = 2,togeneralize=T,pinput = dinput, 
                    colory=c("#330033","#330066","#330099","#3300CC","#3366CC","#336666",
                             "#33FF66","#33FF00", "#FFFF66", "#FFFF33", "#FFFF00",
                             "#FF9933","#FF9900","#FF6600","#FF3300","#FF0000","#660000"))
    })
    
    
    
  })
  #################TMIN###############################
  observeEvent(input$go,{output$UIPLOT_TMIN <- renderUI({ plotOutput("TMIN", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["TMIN"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=-273.15,time = input$bins,rodzaj = "TMIN",start = -30,stop = 40,by = 2,togeneralize=T,pinput = dinput, 
                    colory=c("#330033","#330066","#330099","#3300CC","#3366CC","#336666",
                             "#33FF66","#33FF00", "#FFFF66", "#FFFF33", "#FFFF00",
                             "#FF9933","#FF9900","#FF6600","#FF3300","#FF0000","#660000"))
    })
    
    
    
  })
  
###############DPT####################################
  observeEvent(input$go,{output$UIPLOT_DPT <- renderUI({ plotOutput("DPT", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["DPT"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=-273.15,time = input$bins,rodzaj = "DPT",start = -30,stop = 40,by = 2,togeneralize=T,pinput = dinput, 
                    colory=c("#330033","#330066","#330099","#3300CC","#3366CC","#336666",
                             "#33FF66","#33FF00", "#FFFF66", "#FFFF33", "#FFFF00",
                             "#FF9933","#FF9900","#FF6600","#FF3300","#FF0000","#660000"))
    })
    
  })
  ######
  observeEvent(input$go,{output$UIPLOT_RH <- renderUI({ plotOutput("RH", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["RH"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=0,time = input$bins,rodzaj = "RH",start = 0,stop = 100,by = 10,togeneralize=T,pinput = dinput, 
                    colory=c("#D1FFF7", "#B3F6CC", "#A0EE97", "#B9E57D", "#DDD764", "#D48E4D", "#CC3837", "#C3236E", "#BB11B1","#6800B2"))
    })
    
  })
  ##############
  
  observeEvent(input$go,{output$UIPLOT_PRMSL <- renderUI({ plotOutput("PRMSL", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["PRMSL"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=0,time = input$bins,rodzaj = "PRMSL",start = 0,stop = 100,by = 10,togeneralize=T,pinput = dinput, 
                    colory=c("#D1FFF7", "#B3F6CC", "#A0EE97", "#B9E57D", "#DDD764", "#D48E4D", "#CC3837", "#C3236E", "#BB11B1","#6800B2"))
    })
    
  })
  
  
  observeEvent(input$go,{output$UIPLOT_GUST <- renderUI({ plotOutput("GUST", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["GUST"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=0,time = input$bins,rodzaj = "GUST",start = 0,stop = 100,by = 10,togeneralize=T,pinput = dinput, 
                    colory=c("#D1FFF7", "#B3F6CC", "#A0EE97", "#B9E57D", "#DDD764", "#D48E4D", "#CC3837", "#C3236E", "#BB11B1","#6800B2"))
    })
    
  })
  
  
  
  
  
  observeEvent(input$go,{output$UIPLOT_CAPE <- renderUI({ plotOutput("CAPE", height = 650, width = 650)})})
  
  observeEvent(input$go, {
    
    output[["CAPE"]] <- renderPlot({
      dinput <- isolate(paste(gsub("-","",input$date),input$Hour, sep=""))
      create_raster(minus=0,time = input$bins,rodzaj = "CAPE",start = 0,stop = 2500,by = 10,togeneralize=T,pinput = dinput, 
                    colory=c("white","#D1FFF7", "#B3F6CC", "#A0EE97", "#B9E57D", "#DDD764", "#D48E4D", "#CC3837", "#C3236E", "#BB11B1","#6800B2"))
    })
    
  })

  
  #tutaj będą dane z temperaturą powietrza
  output$widget_temp <- renderPlot({
    m <- input$bins 
    
    tmp.slice <- tmp.array[, , m]
    grid <- expand.grid(lon = lon, lat = lat)
    
    lonlat <- expand.grid(lon, lat)
    tmp.vec <- as.vector(tmp.slice)
    length(tmp.vec)
    
    
    tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
    names(tmp.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
    pts <- tmp.df01
    plot(tmp.df01$SBCAPE, type="line")
    
  })
  
})
