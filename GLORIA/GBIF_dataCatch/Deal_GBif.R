library(data.table)
library(rgbif)
library(ggplot2)
path <- "E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/" 
sp_p <- list.files("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result", pattern='.csv')
setwd(paste0(path,"prim_result"))
my_data <- lapply(sp_p,fread)
loss <- sub('.csv',"",sp_p[lapply(my_data,nrow)<30])
loss_file <- lapply(1:length(loss),FUN=function(x){
  paste0(loss[x],'.csv')})

write.csv(loss,"E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/loss_name.csv")
###### next step:checking the name of the GBif to find out why are the obs so low?
####### move to the GetGBIF and re-download the corrected data from GBif
#################### remove the incomplete data
for(i in 2:length(loss)){
  file.copy(paste0(path,"prim_result/",loss_file[i]),paste0(path,
                                                            "loss_file/",loss_file[i]))
  file.remove(paste0(path,"prim_result/",loss_file[i]))
}
################### Starting to organize data
sp_p <- list.files("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result", pattern='.csv')
sp_n <- sub('.csv',"",sp_p)
setwd(paste0(path,"prim_result"))
my_data <- lapply(sp_p,fread)
for (i in 1:length(sp_n)){
  my_data[[i]][,name:=sp_n[i]][,code:=as.numeric(i)]
}
bio <- NULL
bio_tol <- NULL
for (i in 1:73){
bio <- my_data[[i]]
bio_tol <- rbind(bio_tol,bio)
}
bio_tol <- bio_tol[bio_1!="NA"]
write.csv(bio_tol,"species_climate.csv")
#############################plot
ggplot(bio_tol,aes(factor(code),bio_1))+
  geom_boxplot()
###########################
dt <- bio_tol[,.SD,.SDcols=patterns('bio')]
bio_tol[,names(dt):=lapply(.SD,as.numeric),.SDcols=patterns('bio')]
dt2 <- bio_tol[,quantile(.(bio_1,bio_12)),by=name]
dt2[,q:=as.factor(rep(1:5,times=73))]
dt2 <- dcast(dt2,name~q,value.var = 'V1')
colnames(dt2)[2:6] <- paste0("bio12_",colnames(dt2)[2:6])
write.csv(dt2,"E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/bio_12_qun.csv")

