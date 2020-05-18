library(data.table)
library(rgbif)
setwd("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result")
sp_p <- list.files("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result", pattern='.csv')
my_data <- lapply(sp_p,fread)
loss <- sub('.csv',"",sp_p[lapply(my_data,nrow)<30])
my_data[lapply(my_data,nrow)<30]
write.csv(loss,"E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/loss_name.csv")
################ checking the loss name and completing the data



