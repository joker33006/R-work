library(data.table)
library(raster)
library(tictoc) # running time
library(parallel) #平行運算
library(ggplot2)
library(agricolae)

path <- "E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/" 
sv_path <- paste0(path,"warmth_index/")
rast_path <- 'E:/Climdata/Chelsa_1979-2013_monthly/'
sp_p <- list.files("E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/prim_result", pattern='.csv')
my_data <- lapply(paste0(path,"prim_result/",sp_p),fread)
rast_name <- list.files(rast_path)
fin_r <- NULL
for (j in 1:length(my_data)){
  data <- my_data[[j]]
  data[,key_uni:=paste0(decimalLatitude,decimalLongitude)]
  setkey(data,key_uni)
  data <- unique(data)
  result <- NULL
  for (i in 1:length(rast_name)){
    tic()
      rast <- raster(paste0('E:/Climdata/Chelsa_1979-2013_monthly/',rast_name[i]))
      spot <- data[decimalLatitude!="NA",.(decimalLongitude,decimalLatitude,key)]
      dot <- spot[,.(decimalLongitude,decimalLatitude)]
      x <- extract(rast,dot)
      result <-cbind(result,x) 
      colnames(result)[i] <- paste0("mon_",i)
  }
  result <- as.data.table(result)
  result[,scientificname:=sub(".csv","",sp_p[j])][,code:=j]
  result <- cbind(spot,result)
  result <- result[mon_1!='NA']
  r2 <- result[,4:15]
  r2 <- r2-50
  r2 <- r2*(r2>0)
 result[,WI:=rowSums(r2)/10]  
 fin_r <- rbind(fin_r,result[,.(code,scientificname,WI)])
 write.csv(result,paste0(sv_path,sp_p[j]))
 cat(j)
 toc()
}
write.csv(fin_r,paste0(path,"WI_data_base.csv"))
######################################plot and group
fin_r <- fread(paste0(path,"WI_data_base.csv"))
tuk <- with(fin_r,Median.test(WI,code))
order <-cbind(tuk$groups,rownames(tuk$groups))
colnames(order)[3] <- "code"
order <- as.data.table(order)
order[,code:=as.character(code)]
fin_r[,code:=as.character(code)]
fin_r <- fin_r[order,on=.(code=code)]

ggplot(data=fin_r,aes(x=reorder(code,-i.WI),y=WI,fill=groups))+
  geom_boxplot(outlier.colour=NA)+
  theme_classic()+
  labs(x="code",y='Warmth Index')+
  geom_hline(yintercept=c(12,36,72,108,144,216), linetype="dashed", color = "gray51")
ggsave(paste0(path,"plot_result/bio_WI.jpeg"),width = 12,height = 6,dpi=600)
###############calculate the quantile
q1 <- fin_r[,quantile(WI),by=code]
q1 <- q1[,q:=rep(1:5,times=nrow(dt2)/5)]
q1 <- dcast(q1,code~q,value.var = "V1")
write.csv(q1,paste0(path,'species_WI_niche.csv'))
