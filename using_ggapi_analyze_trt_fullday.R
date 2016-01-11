library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggmap)
library(rjson)
# install.packages("bitops")

decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}





file1 ="C:/Users/62552/Desktop/台中交控案/台中五權路etag點位資料.csv"

gate_xy_tc<-read.table(file1, header=T,sep = ",",encoding ="big5")
gate_xy_tc$site<- iconv(gate_xy_tc$site, "big5", "utf8") 

library(gdata)

?combn()
gate_xy_tc$station<-as.character(gate_xy_tc$station)
west_station<-c("fetag-01","fetag-02","fetag-03","fetag-04","fetag-05")
east_station<-c("fetag-06","fetag-07","fetag-08")



west_od <- combn(west_station, 2)
east_od <- combn(east_station, 2)
west_od_t<-t(west_od)
east_od_t<-t(east_od)
dir_w<-c("出城")
dir_e<-c("入城")
west_od_t<-c(west_od_t,dir_w)
east_od_t<-c(east_od_t,dir_e)

west_od_t<-as.data.frame(west_od_t)
east_od_t<-as.data.frame(east_od_t)
colnames(west_od_t)<-c('o_station','d_station','dir')
colnames(east_od_t)<-c('o_station','d_station','dir')
od_patt<-rbind(west_od_t,east_od_t)

num=nrow(od_patt)

dur_time_finall<-c()
dur_time_finall_total_w<-c()


for(i in 1:num){

    point_od<-od_patt[i,]
  
  o_point=as.character(point_od$o_station)
  d_point=as.character(point_od$d_station)
  
  
  o_lon<-(filter(gate_xy_tc,station==o_point))$lon
  o_lat<-(filter(gate_xy_tc,station==o_point))$lat
  d_lon<-(filter(gate_xy_tc,station==d_point))$lon
  d_lat<-(filter(gate_xy_tc,station==d_point))$lat
  
  from<-paste(as.character(o_lon),as.character(o_lat),sep=", ")
  from<-as.character(from)
  to<-paste(as.character(d_lon),as.character(d_lat),sep=", ")
  to<-as.character(to)
  
  origin<-from
  destination<-to
  
  travelMode <- "driving"  
    

  for(j in 1:47){

      tttt<-as.POSIXct("2016-01-19 00:00:00")
  tttt<-as.POSIXct((as.numeric(tttt)+1800*(j-1)),origin="1970-01-01")
tttt
# departureTime <- Sys.time() #I want to leave now!
departureTime <- tttt 

key="AIzaSyBx8Q6LSdNF5EhAmH9_BrJyyVCq1uFTGFI"
baseUrl <- "https://maps.googleapis.com/maps/api/directions/json?"
finalUr1 <- paste(baseUrl
                  , "origin=", origin
                  , "&destination=", destination
                  , "&sensor=false"
                  , "&mode=", travelMode
                  , "&departure_time=", as.integer(departureTime)
                  , "&key=",key 
                  , sep = ""
)

# finalUr1 <- paste(baseUrl
#                   , "origin=", origin
#                   , "&destination=", destination
#                   , "&sensor=false"
#                   , "&mode=", travelMode
#                   , "&departure_time=", as.integer(departureTime)
#                   , sep = ""
# )
# finalUr1
url_string <- URLencode(finalUr1)
trip <- fromJSON(paste(readLines(url_string), collapse = ""))

tripPathEncoded <- trip$routes[[1]]$overview_polyline$points

k=length(trip$routes[[1]]$legs[[1]]$steps[])
 dur_time<-trip$routes[[1]]$legs[[1]]$duration_in_traffic$value
# dur_dist<-trip$routes[[1]]$legs[[1]]$distance$value
# duration_in_traffic
 dur_time_finall<-c(dur_time_finall,dur_time)
# dur_dist
 Sys.sleep(0.1) 
 
  } 
  
  dur_time_finall_total_w<-cbind(dur_time_finall_total_w,dur_time_finall)
  dur_time_finall<-c()
  
}
ggapi_dur_time_mtx_w<-as.data.frame(dur_time_finall_total_w)
ggapi_dur_time_mtx<-as.data.frame(dur_time_finall_total)



  ff=c(1,2,3,4)
  fff<-ff
  fff<-cbind(fff,ff)
  ffff<-cbind(fff,ff)
ffff  
gmap_trip_trt=0
gmap_trip_dis=0

for(i in 1:k){
  tripdurationsteptime<- trip$routes[[1]]$legs[[1]]$steps[[i]]$duration$value
  gmap_trip_trt<-gmap_trip_trt+tripdurationsteptime
  tripdurationstepdis <- trip$routes[[1]]$legs[[1]]$steps[[i]]$distance$value
  gmap_trip_dis<-gmap_trip_dis+tripdurationstepdis
}

tripPathCoords <- decodeLine(tripPathEncoded)

centerpoint="24.139858, 120.649200"

map <- get_map(centerpoint, zoom = 14
)
ggmap(map) +
  geom_path(aes(lon, lat), data=tripPathCoords, lwd = 2)


}


str(m[,1])
plot(1:3)
grid(NA, 5, lwd = 2) # grid only in y-direction
combn(c(1,1,1,1,2,2,2,3,3,4), 3, tabulate, nbins = 4)
as.c


