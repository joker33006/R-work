library(rgbif)
library(data.table)
library(raster)
setwd("E:/忍者/R-work/GLORIA/GBIF_dataCatch")
svpath <- "E:/忍者/R-work/GLORIA/GBIF_dataCatch"
path <- 'E:/Climdata/Chelsa_current_2019'
clim_d_n <- list.files(path)
nlist <-read.csv("name_list.txt")
for (j in 67:length(nlist[,1])){
  name <- as.character(nlist[j,1])
  ##### start extract the climate data
  result <- NULL
  for (i in 1:length(clim_d_n)){
    rast <- raster(paste0(path,'/',clim_d_n[i]))
    data <- as.data.table(occ_search(scientificName = name)$data)
    if (nrow(data)==0){name <- paste0('NULL_',name)
    break
    }else{
      spot <- data[decimalLatitude!="NA",.(decimalLongitude,decimalLatitude)]
      x <- extract(rast,spot)
      result <-cbind(result,x) 
     colnames(result)[i] <- paste0("bio_",i)
     }
  }
write.csv(result,paste0(svpath,"/prim_result/",name,".csv"))
cat(j)
}
#deal with the 