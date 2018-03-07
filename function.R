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
read_delim(gzfile(tmp),delim=",", col_names = c("V1","Date","Type","V4","x","y","z"), col_types='TTccddd') %>%
  dplyr::select("Date","Type","x","y","z")%>%
  dplyr::filter(Type==type)#%>%
  #dplyr::select("Date","x","y","z")
  })
}


create_raster <- function(minus,reduce,time, rodzaj, start, stop, by=5, colory, pinput){
#profvis({
  
  
  if(missing(reduce)==T){
    reduce <- 1
  }
  
  
  if(exists(rodzaj,where=dane)==FALSE || pinput!=wybranyczas){
    
    
    dane[[rodzaj]] <<- file_download(date_start_model = pinput, type = rodzaj)
    dane[[rodzaj]]$Date <<- as.POSIXct(strptime(dane[[rodzaj]]$Date, "%Y-%m-%d %H:%M:%S"))
  } 
  
   templist <<-  list(czas=0, abc=NULL,a=NULL, r_temp=NULL)
  
   templist$czas <<- dane[[rodzaj]]$Date[1]+time*60*60
  cape_time <<- dane[[rodzaj]] %>%
    dplyr::filter(Date==templist$czas)#%>%
   # dplyr::filter(x %in% seq(14.00, 25, .50))
    

  
  
  
  cape_time$z <<- cape_time$z/reduce+minus
  
  rasterlist <<- NULL

  rasterlist$xyz <<- cape_time[c(3,4,5)]
  rasterlist$ln <<- colnames(rasterlist$xyz)
  rasterlist$xyz <<- as.matrix(rasterlist$xyz)
  #xyz <- matrix(as.numeric(xyz), ncol=ncol(xyz), nrow=nrow(xyz))
  
  rasterlist$x <<- sort(unique(rasterlist$xyz[,1]))
  rasterlist$dx <<- rasterlist$x[-1] - rasterlist$x[-length(rasterlist$x)]
  rasterlist$rx <<- min(rasterlist$dx)
    
  rasterlist$y <<- sort(unique(rasterlist$xyz[,2]))
  rasterlist$dy <<- rasterlist$y[-1] - rasterlist$y[-length(rasterlist$y)]
  rasterlist$ry <<- min(rasterlist$dy)
  
  rasterlist$minx <<- 14
  rasterlist$maxx <<- 25
  rasterlist$miny <<- 47.7
  rasterlist$maxy <<- 55.3

  rasterlist$d <<- dim(rasterlist$xyz)
  rasterlist$r <<- raster(xmn=rasterlist$minx, xmx=rasterlist$maxx, ymn=rasterlist$miny, ymx=rasterlist$maxy, crs="+init=epsg:4326")
    res(rasterlist$r) <<- c(rasterlist$rx, rasterlist$ry)
    rasterlist$cells <<- cellFromXY(rasterlist$r, rasterlist$xyz[,1:2])
    names(rasterlist$r) <<- rasterlist$ln[-c(1:2)]
    rasterlist$r[rasterlist$cells] <<- rasterlist$xyz[,3:rasterlist$d[2]]


#linelist <<- NULL
    #x <<- sampleRegular(rasterlist$r, size=1000000, asRaster=TRUE, useGDAL=TRUE)
   # linelist$cL <<- grDevices::contourLines(x=xFromCol(x,1:ncol(x)), y=yFromRow(x, nrow(x):1), z=t((getValues(x, format='matrix'))[nrow(x):1,]))
    #linelist$cLstack <<- tapply(1:length(linelist$cL), sapply(linelist$cL, function(x) x[[1]]), function(x) x, simplify = FALSE)
    

  
#  linelist$df <<- data.frame(level = names(linelist$cLstack))
 # linelist$m <<- length(linelist$cLstack)
 # linelist$res <<- vector(mode = "list", length = linelist$m)
 # linelist$IDs <<- paste("C", 1:linelist$m, sep = "_")
 # row.names(linelist$df) <<- linelist$IDs
 # for (i in 1:linelist$m) {
 #   linelist$res[[i]] <<- Lines(.contourLines2LineList(linelist$cL[linelist$cLstack[[i]]]), ID = linelist$IDs[i])
  #}
  
#  linelist$SL <<- SpatialLines(linelist$res, proj4string = projection(x, asText=FALSE))
 # templist$a <<- SpatialLinesDataFrame(linelist$SL, data = linelist$df)
  
  colfunc <<- colorRampPalette(colory)
  
  wybranyczas <<- pinput 

 tm_shape(rasterlist$r) +
    tm_raster(palette = colfunc(80), auto.palette.mapping = FALSE, interpolate = T, breaks = seq(start,stop, by),style="cont",legend.is.portrait=FALSE,
      title="") +
   tm_layout(title.bg.color = "white",
             legend.show = T,attr.outside=F, legend.stack = "horizontal",attr.position = c("right", "top"),
             attr.just = c("top","left"))+
   tm_legend(height=0.5,width=2.5,
            legend.text.size = 2,
              position=c("center", 0.02),
              frame=F,
              bg.color="white") +
   tm_layout(title=paste(rodzaj,format(templist$czas,"%Y-%m-%d %H:%M")), inner.margins=c(.0, .0, 0, .0), title.size = 2)+
    tm_scale_bar(size = 2)+
   tm_shape(Europe, unit = "km") +
    tm_polygons(alpha = 0, border.col = "white", lwd=2)+
   #tm_shape(templist$a)+
   #tm_lines(col = "grey",lwd = 1)+
    #tm_text("level", size = 1.3)+
    tm_xlab("latitude", size = 2, rotation = 0 )+
    tm_ylab("longitude", size = 2, rotation = 90)+
    tm_grid(x = c(15,18,21,24),y=c(50,52,54), labels.inside.frame=F, col = "black", labels.size = 2.5)+
     tm_credits(text = "Create by Mariusz Kupczyk",position = c("center", 0.12), size = 2)
   
  
 

####    
#})
 ##### 
  
}

legend_to_map <- function(col_n=80,start=-40, stop=40, by=1, by_leg=5, colory){
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



.contourLines2LineList <- function(cL) {
  list_line <<- NULL
  list_line$n <- length(cL)
  list_line$res <- vector(mode="list", length=list_line$n)
  for (i in 1:list_line$n) {
    list_line$crds <- cbind(cL[[i]][[2]], cL[[i]][[3]])
    list_line$res[[i]] <- Line(coords=list_line$crds)
  }
  list_line$res
}



profil <- function(minus, reduce, time, rodzaj, lon, lat, x, y){
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


file_download <- cmpfun(file_download)
create_raster <- cmpfun(create_raster)
.contourLines2LineList <- cmpfun(.contourLines2LineList)