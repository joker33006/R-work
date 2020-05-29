library(rgbif)
library(data.table)
library(raster)
library(tictoc) # running time
library(parallel) #平行運算
###### setup the function
gbif_and_clim <- function(name){
  require(data.table)
  require(rgbif)
  require(raster)
  require(tictoc)
  result <- NULL
  data <- as.data.table(occ_search(scientificName = name)$data)
  clim_d_n <- list.files('E:/Climdata/Chelsa_current_2019')
  for (i in 1:length(clim_d_n)){
    rast <- raster(paste0('E:/Climdata/Chelsa_current_2019/',clim_d_n[i]))
    if (nrow(data)==0){name <- paste0('NULL_',name)
    break
    }else{
      spot <- data[decimalLatitude!="NA",.(decimalLongitude,decimalLatitude,key)]
      dot <- spot[,.(decimalLongitude,decimalLatitude)]
      x <- extract(rast,dot)
      result <-cbind(result,x) 
      colnames(result)[i] <- paste0("bio_",i)
    }
  }
  try(result <- cbind(spot[,.(decimalLongitude,decimalLatitude,key)],result))
  write.csv(result,paste0("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result/",name,".csv"))
}
#####################end function

setwd("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch")
svpath <- "E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch"
nlist <-read.csv("name_list.txt")
cl <- makeCluster(8)
for (j in 1:length(nlist[,1])){
  j=1
  tic("total")
    name <- as.character(nlist[j,1])
  ##### start extract the climate data
  parLapply(cl,name,gbif_and_clim)
cat(j)
toc()
}
stopCluster(cl)
#deal with the primary data

gbif_and_clim(name)
