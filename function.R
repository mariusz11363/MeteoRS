file_download <- function(date_start_model, type){
 ############################################################# fread  
  if(exists("wybranyczas")==FALSE){
    wybranyczas <- 0
  }
  
  if(exists("dane")==FALSE || date_start_model!=wybranyczas){

tmp <<- tempfile()
withProgress(message = 'Download data', value = 0, {
  incProgress(1/1, detail = paste("Pleas wait"))
download.file(paste("http://enwo.pl/kuba/gfs.",date_start_model,".gz", sep=""), tmp)
  })

  }
  withProgress(message = 'Caltulate data!', value = 0, {
    incProgress(1/1, detail = paste("Pleas wait"))
taempread <- read_delim(gzfile(tmp),delim=",", col_names = c("V1","Date","Type","V4","x","y","z"), col_types='TTccddd') %>%
  dplyr::select("Date","Type","x","y","z")%>%
  dplyr::filter(Type==type)#%>%
  #dplyr::select("Date","x","y","z")
  })
}


create_raster <- function(minus,time, rodzaj, start, stop, by, colory, togeneralize, pinput){
 #profvis({
  
  if(exists(rodzaj,where=dane)==FALSE || pinput!=wybranyczas){
    
    
    dane[[rodzaj]] <<- file_download(date_start_model = pinput, type = rodzaj)
    dane[[rodzaj]]$Date <<- as.POSIXct(strptime(dane[[rodzaj]]$Date, "%Y-%m-%d %H:%M:%S"))
  } 
  
  
  czas <<- dane[[rodzaj]]$Date[1]+time*60*60
  abc <<- NULL 
  #colory <- c("white","cyan","green","yellow","orange", "red", "#600000")
  cape_time <<- dane[[rodzaj]] %>%
    dplyr::filter(Date==czas)
    
    
    if(togeneralize==T){
    generalization()
      cape_time <<- cape_time[-abc,]
    }
    
    
  cape_time$z <- cape_time$z+minus
  r_temp <- rasterFromXYZ(cape_time[c(3,4,5)])
  proj4string(r_temp) <- CRS("+init=epsg:4326")
  a <- raster::rasterToContour(r_temp)
  
  wybranyczas <<- pinput 
  
  tm_shape(r_temp, n.x = 5) +
    tm_raster(n=50,palette = colory, auto.palette.mapping = FALSE, interpolate = T, breaks = seq(start,stop, by),
              title="", legend.show = F)+ 
    tm_format_Europe(title = NA, title.position = c("left", "top"),attr.outside=T,legend.outside=TRUE,
                     legend.text.size = 1.5,legend.outside.position=c("left"),
                     attr.position = c("right", "bottom"), legend.frame = TRUE,
                     inner.margins = c(.0, .0, 0, 0))+tm_scale_bar(size = 1)+
    tm_shape(Europe, unit = "km") +
    tm_polygons(alpha = 0, border.col = "white", lwd=2, title=paste(format(czas,"%Y-%m-%d %H:%M")))+
    tm_shape(a)+
    tm_lines(col = "grey",lwd = 1)+
    tm_text("level", size = 1.3)+
    tm_xlab("latitude", size = 1, rotation = 0 )+
    tm_ylab("longitude", size = 1, rotation = 90)+
    tm_grid(x = c(16,24),y=c(50,55), labels.inside.frame=F, col = "black", labels.size = 1)+
    tm_layout(title = paste(format(czas,"%Y-%m-%d %H:%M")), main.title.position = c("left", "bottom"), title.bg.color = "white")
####    
#})
 ##### 
}


generalization <- function(){
  for(i in seq(14.25, 25, .50)){
    if(exists("abc")==T){
      abc <<-c(abc,which(cape_time$x == i))
    }
    else{
      abc <<- which(cape_time[,1] == i)
    }
  }}



#file_download <- cmpfun(file_download)
#create_raster <- cmpfun(create_raster)
#generalization <- cmpfun(generalization)
