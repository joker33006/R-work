library(data.table)
library(rgbif)
library(ggplot2)
setwd("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result")
sp_p <- list.files("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result", pattern='.csv')
my_data <- lapply(sp_p,fread)
loss <- sub('.csv',"",sp_p[lapply(my_data,nrow)<30])
write.csv(loss,"E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/loss_name.csv")
################ checking the loss name and completing the data
sp_n <- sub('.csv',"",sp_p)
for (i in 1:72){
  my_data[[i]][,name:=sp_n[i]][,code:=as.numeric(i)]
}
bio <- NULL
bio_tol <- NULL
for (i in 1:72){
bio <- my_data[[i]]
bio_tol <- rbind(bio_tol,bio)
}
bio_tol <- bio_tol[bio_1!="NA"]
#############################plot
ggplot(bio_tol,aes(factor(code),bio_12))+geom_boxplot()
class(bio_tol[,code])
