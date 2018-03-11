load("dane.RData")
create_raster <- function(time,start, stop, by=1, colory, type){
  #profvis({
  enableJIT(3)
  colfunc <- colorRampPalette(colory)

  
 tm_shape(dane_lista[[type]]$r[[time]]) +
    tm_raster(palette = colfunc(80), auto.palette.mapping = FALSE, interpolate = T, style = "cont",breaks = seq(start,stop, by),legend.is.portrait=FALSE,
      title="") +
   tm_layout(title.bg.color = "white",title=paste(type,dane_lista$czas[time]), inner.margins=c(.0, .0, 0, .0), title.size = 2,
             legend.show = F,attr.outside=F, legend.stack = "horizontal",attr.position = c("right", "top"),
             attr.just = c("top","left"))+
   tm_legend(height=0.5,width=2.5,
            legend.text.size = 2,
              #position=c("center", 0.02),
              frame=F,
              bg.color="white") +
    tm_scale_bar(size = 2)+
   tm_shape(Europe, unit = "km") +
    tm_polygons(alpha = 0, border.col = "white", lwd=2)+
   
   tm_shape(dane_lista[[type]]$c[[time]])+
   tm_lines(col = "grey",lwd = 2, breaks = seq(start,stop, by))+
   tm_text("level", size = 2, breaks = seq(start,stop, by))+
   
    tm_xlab("latitude", size = 2, rotation = 0 )+
    tm_ylab("longitude", size = 2, rotation = 90)+
    tm_grid(x = c(15,18,21,24),y=c(50,54), labels.inside.frame=F, col = "black", labels.size = 2.5)+
     tm_credits(text = "Create by Mariusz Kupczyk - MeteoRS",position = c("center", 0.02), size = 2)
   
  
 

####    
#})
 ##### 
  
}

legend_to_map <- function(col_n=80,start=-40, stop=40, by=1, by_leg=5, colory){
  enableJIT(1)
  colfunc1 <<- colorRampPalette(colory)
  par(oma=c(0,0,0,0))
  ncol <- length(colfunc1(col_n))
  z <- matrix((utils::head(seq(start, stop, by), ncol) + utils::tail(seq(start, stop, by), ncol))/2)
  
  graphics::image(seq(start, stop, by), 0:1, z, xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
                  col = colfunc1(col_n), breaks = seq(start, stop, by))
  axis.args <- list()
  axis.args$at <- seq(start, stop, by_leg)
  axis.args$side <- ifelse(T, 1, 4)
  axis.args$las <- ifelse(T, 0, 2)
  do.call("axis", axis.args)
  
}




profil <- function(minus, reduce, time, rodzaj, lon, lat, x, y){
  enableJIT(1)
  if(missing(reduce)==T){
    reduce <- 1
  }
  czas <- dane[[rodzaj]]$Date[1]+time*60*60
  cape_time <<- dane[[rodzaj]] %>%
    dplyr::filter(Date==czas) %>%
    dplyr::filter(x %in% 14.25)
  cape_time$z <<- cape_time$z/reduce+minus
  cape_time <- cape_time[order(cape_time$y),]
  

qplot(y=cape_time$z,x=cape_time$y, geom=c("line"))
}


create_raster <- cmpfun(create_raster)
