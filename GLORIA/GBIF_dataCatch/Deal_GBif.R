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
write.csv(bio_tol,paste0(path,"Niche_all_the_raw_data/species_climate.csv"))
#############################plot
name_list <- fread(paste0(path,'Surveydata_and_niche/name_list.csv'))
bio_tol <- fread(paste0(path,"Niche_all_the_raw_data/species_climate.csv"))
bio_tol <- bio_tol[name_list, on =.(code=GBIF_code)]
bio_tol[,bio_1:=bio_1/10]
i='bio_12'
i='bio_1'
ylabs <- c('AAT(℃)','AP(mm)')

tuk <- with(bio_tol,Median.test(get(i),code,alpha=0.01))
order <-cbind(tuk$groups,rownames(tuk$groups))
colnames(order)[3] <- "code"
order <- as.data.table(order)

setnames(order,"groups",paste0(i,'_groups'))
order[,code:=as.integer(code)]
bio_tol <- bio_tol[order,on=.(code=code)]

ggplot(data=bio_tol,aes(x=reorder(Order,-`get(i)`),y=bio_12,fill=bio_12_groups))+
  geom_boxplot(outlier.colour=NA)+
  theme_classic()+
  labs(x="code",y=ylabs[2])+
#  geom_hline(yintercept=c(5,8,11,14,17,23), linetype="dashed", color = "gray51")
 geom_hline(yintercept=c(250,500,1000,2000,4000), linetype="dashed", color = "gray51")
ggsave(paste0(path,"plot/",i,"group.jpeg"),width = 16,height = 6,dpi=600)

ggplot(data=bio_tol,aes(x=reorder(Order,-`get(i)`),y=bio_1,fill=bio_1_groups))+
  geom_boxplot(outlier.colour=NA)+
  theme_classic()+
  labs(x="code",y=ylabs[1])+
 geom_hline(yintercept=c(5,8,11,14,17,23), linetype="dashed", color = "gray51")
  #geom_hline(yintercept=c(250,500,1000,2000,4000), linetype="dashed", color = "gray51")
ggsave(paste0(path,"plot/",i,"group.jpeg"),width = 14,height = 6,dpi=600)

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

