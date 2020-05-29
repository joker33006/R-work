####deal the inat
library(data.table)
library(raster)
rdata <- fread('E:/忍者/R-work/Git/R-work/Chao/iNat/Heloniopsis umbellata.csv')
clim_d_n <- list.files('E:/Climdata/Chelsa_1979-2013_monthly')
result <- NULL
for (i in 1:length(clim_d_n)){
  rast <- raster(paste0('E:/Climdata/Chelsa_1979-2013_monthly/',clim_d_n[i]))
    dot <- rdata[,.(longitude,latitude)]
    x <- extract(rast,dot)
    result <-cbind(result,x) 
    colnames(result)[i] <- paste0("mon_",i)
  }
try(result <- cbind(rdata,result))
write.csv(result,"E:/忍者/R-work/Git/R-work/Chao/iNat/Heloniopsis umbellata.csv")
###deal with the iNat
d <- result
t_r <- NULL

for (k in 1:nrow(d)){
  mon <- as.numeric(d[`k`,month]+match('mon_1',colnames(d)))-1
  temp <- as.numeric(d[`k`,mon,with=FALSE])
  t_r <- c(t_r,temp)
}
r2 <- cbind(d[,1:7],t_r)
write.csv(r2,"E:/忍者/R-work/Git/R-work/Chao/iNat/result_Heloniopsis umbellata.csv")
