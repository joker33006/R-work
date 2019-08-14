library(raster)
path <- "E:/test/future/future50_65"
outpath <- "E:/test/future"
r <- list.files(path)
i=1
for (i in c(1:length(r))){
name <- gsub(".tif","",r[i])

rast <- raster(paste0(path,"/",r[i]))
writeRaster(rast,paste0(outpath,"/",name,".grd"),overwrite=TRUE)
}
