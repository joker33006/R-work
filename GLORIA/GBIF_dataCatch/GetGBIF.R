library(rgbif)
library(data.table)
library(raster)
library(tictoc) # running time
library(parallel) #平行運算
setwd("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch")
svpath <- "E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch"
path <- 'E:/Climdata/Chelsa_current_2019'
clim_d_n <- list.files(path)
nlist <-read.csv("name_list.txt")
cl <- makeCluster(6)
for (j in 1:length(nlist[,1])){
  tic("total")
    name <- as.character(nlist[j,1])
  ##### start extract the climate data
  result <- NULL

  for (i in 1:length(clim_d_n)){
    tic("climate")
    rast <- raster(paste0(path,'/',clim_d_n[i]))
    data <- as.data.table(occ_search(scientificName = name)$data)
    if (nrow(data)==0){name <- paste0('NULL_',name)
    break
    }else{
      spot <- data[decimalLatitude!="NA",.(decimalLongitude,decimalLatitude,key)]
      dot <- spot[,.(decimalLongitude,decimalLatitude)]
      x <- extract(rast,dot)
      result <-cbind(result,x) 
     colnames(result)[i] <- paste0("bio_",i)
     toc()
    }
  }
  result <- cbind(spot[,.(decimalLongitude,decimalLatitude,key)],result)
write.csv(result,paste0(svpath,"/prim_result/",name,".csv"))
cat(j)
toc()

}
#deal with the primary data

