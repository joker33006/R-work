library(sf)
library(adehabitatHR)
library(rgdal)
rm(list=ls())
setwd("C:/Users/user/Desktop/JiaJia")
name_list <- list.files("rawdata_file")
for (i in c(1:length(name_list))){
    kml_d <-st_read(paste0("rawdata_file/",name_list[i]))
    xy <-SpatialPoints(do.call(rbind, st_geometry(kml_d))[,1:2])
  xy.mcp <- mcp(xy,percent = 95)
  xy.kd <-kernelUD(xy, h="href")
  xy.kd50 <- getverticeshr(xy.kd,percent = 50)

  xy.kd95 <- getverticeshr(xy.kd,percent = 95)
  r_name <- gsub(".kml","",name_list[i])
   writeOGR(xy.kd50,"result",layer=paste0(r_name,"_kd50"),
           driver="ESRI Shapefile")
   writeOGR(xy.mcp,"result",layer=paste0(r_name,"_mcp95"),
            driver="ESRI Shapefile")
   writeOGR(xy.kd95,"result",layer=paste0(r_name,"_kd95"),
            driver="ESRI Shapefile")
 
  }

