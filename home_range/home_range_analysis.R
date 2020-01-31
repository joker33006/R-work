library(sf)
library(adehabitatHR)
library(rgdal)
library(raster)
rm(list=ls())
setwd("C:/Users/user/Desktop/JiaJia")
name_list <- list.files("rawdata_file")
area_c <- NULL
area_r <- NULL
for (i in c(1:length(name_list))){
  i=2
  kml_d <-st_read(paste0("rawdata_file/",name_list[i]))
    xy <-SpatialPoints(do.call(rbind, st_geometry(kml_d))[,1:2],proj4string=CRS("+init=epsg:4326"))
  xy.mcp <- mcp(xy,percent = 95)
  xy.kd <-kernelUD(xy, h="href")
  xy.kd50 <- getverticeshr(xy.kd,percent = 50)
 
  xy.kd95 <- getverticeshr(xy.kd,percent = 95)
  r_name <- gsub(".kml","",name_list[i])
  area_c <- c(r_name,area(xy.mcp), area(xy.kd95), area(xy.kd50))
  area_r <- rbind(area_r,area_c)
  writeOGR(xy.kd50,"result",layer=paste0(r_name,"_kd50"),
           driver="ESRI Shapefile",delete_dsn=T)
   writeOGR(xy.mcp,"result",layer=paste0(r_name,"_mcp95"),
            driver="ESRI Shapefile",delete_dsn=T)
   writeOGR(xy.kd95,"result",layer=paste0(r_name,"_kd95"),
            driver="ESRI Shapefile",delete_dsn=T)
 
}
colnames(area_r) <- c("name","MCP95","KD95","KD50")
write.csv(area_r,"area_result.csv")
