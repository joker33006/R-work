library(data.table)
library(rgbif)
library(ggplot2)
path <- "E:/忍者/GLORIA_個人處理/GBIF_data/" 
sp_p <- list.files(paste0(path,"prim_result_total_rd"), pattern='.csv')
my_data <- lapply(sp_p,function(x){(fread(paste0(path,"prim_result_total_rd/",x)))})
loss <- sub('.csv',"",sp_p[lapply(my_data,nrow)<30])
loss_file <- lapply(1:length(loss),FUN=function(x){
  paste0(loss[x],'.csv')})

write.csv(loss,paste0(path,"loss_name.csv"))
###### next step:checking the name of the GBif to find out why are the obs so low?
####### move to the GetGBIF and re-download the corrected data from GBif


sp_n <- sub('.csv',"",sp_p)
#write.csv(sp_n,paste0(path,'name_check.csv')) #used to name_check
my_data <- lapply(sp_p,function(x){fread(paste0(path,"prim_result_total_rd/",x))})
for (i in 1:length(sp_n)){
  my_data[[i]][,name:=sp_n[i]][,code:=as.numeric(i)]
}

bio_tol <- rbindlist(my_data,use.names=T,fill=T)

bio_tol <- bio_tol[bio_1!="NA"|bio_4!=0]
bio_tol <- bio_tol[,V1:=NULL]
bio_tol[,key_L:=paste0(decimalLongitude,decimalLatitude)]
setkey(bio_tol,key_L)
bio_tol<- unique(bio_tol)
write.csv(bio_tol,paste0(path,"species_climate.csv"))
#############################plot
ggplot(bio_tol,aes(factor(code),bio_1))+
  geom_boxplot()
###########################
fac <- c(1,6,12)
result <- NULL
n <- bio_tol[,.N,by=.(name,code)][order(code)]
for (i in c(1,12)){
  dt2 <- bio_tol[order(code)][,quantile(get(paste0('bio_',i))),by=code]
  dt2[,q:=as.factor(rep(1:5,times=nrow(dt2)/5))]
  dt2 <- dcast(dt2,code~q,value.var = 'V1')
  colnames(dt2)[2:6] <- paste("bio",i,colnames(dt2)[2:6],sep = '_')

  result <- cbind(result,dt2)
}

result <-n[result, on=.(code=code)]
write.csv(result,paste0(path,'/bio_niche.csv'))

