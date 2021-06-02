library(rgbif)
library(data.table)
library(raster)

library(tictoc) # running time
library(parallel) #?t??
###### setup the function

gbif_and_clim <- function(name,climpath,data_path){
  library(data.table)
  library(rgbif)
  library(raster)
  library(tictoc)
  result <- NULL
    data <- as.data.table(occ_search(scientificName = name, hasGeospatialIssue=FALSE)$data)
  clim_d_n <- list.files(climpath)
  for (i in 1:length(clim_d_n)){
    rast <- raster(paste0(climpath,clim_d_n[i]))
    if (nrow(data)==0|!any(colnames(data)=='decimalLongitude')){name <- paste0('NULL_',name)
    break
    }else{
      spot <- data[decimalLatitude!="NA",.(decimalLongitude,decimalLatitude,key,scientificName,year,month,day)]
      dot <- spot[,.(decimalLongitude,decimalLatitude)]
      x <- extract(rast,dot)
      result <-cbind(result,x) 
      colnames(result)[i] <- paste0("mon_",i)
    }
  }
  try(result = cbind(spot,result))
  write.csv(result,paste0(data_path,name,".csv"))
}
#####################ending function 

setwd("E:/GitHub/R-work/Chao")

svpath <- "prim_result"
nlist <-read.csv("name_list.txt")
cl <- makeCluster(8)
for (j in 1:length(nlist[,1])){
  tic("total")
  #name <- as.character(nlist[j,1])
  ##### start extract the climate data
  name <- 'Helonias bullata'
  parLapply(cl,name,gbif_and_clim)
  cat(j)
  toc()
}
stopCluster(cl)
#deal with the primary data

gbif_and_clim(name,climpath,data_path)
############################### special process
result <- NULL
climpath <- 'E:/忍者/Climdata/Chelsa_1979-2013_monthly/'
clim_d_n <- list.files(climpath)
name <- "Heloniopsis leucantha"
spot <- fread(paste0('E:/Git_R_work/R-work/Chao/point/',name,'.csv'))
for (i in 1:length(clim_d_n)){
    rast <- raster(paste0(climpath,clim_d_n[i]))

    dot <- spot[,.(decimalLongitude,decimalLatitude)]
    x <- extract(rast,dot)
    result <-cbind(result,x) 
    colnames(result)[i] <- paste0("mon_",i)
}
result <-cbind(dot,result)
write.csv(result,paste0('E:/Git_R_work/R-work/Chao/prim_result/',name,'.csv'))
###########deal with data
path <- "E:/Git_R_work/R-work/Chao/"
svpath <- "E:/Git_R_work/R-work/Chao/prim_result/"
file_name <- list.files(paste0(path,"prim_result/"),pattern = '.csv')
base <- fread(paste0(path,'letter_and_GBIF/phenology.csv'))

rdata <- lapply(paste0(path,"prim_result/",file_name),fread)
rdata <- lapply(1:length(file_name),function(i){
  rdata[[i]][,name:=sub('.csv','',file_name[i])]
})
result_mon <- NULL
for(i in 1:length(file_name)){
  name <- rdata[[i]][1,name]
  j <-which(base[,1]==name)[1]
  st <- as.numeric(base[`j`,2])
  ed <- as.numeric(base[`j`,3])
    for (k in st:ed){
      r <- rdata[[i]][,.(name,.SD),.SDcols=paste0('mon_',k)]
      r[,mon:=k]
      colnames(r)[2] <- 'Temp'
      result_mon <- rbind(result_mon,r)
    cat(paste("k=",k))
    }
cat(i)
}
write.csv(result_mon,paste0(path,"result_20210413.csv"))
##############################deal with the specimen
setwd("D:/GitHub/R-work/Chao/prim_result/")
rdata <- lapply(file_name,fread)
rdata <- lapply(1:length(file_name),function(i){
  rdata[[i]][,name:=sub('.csv','',file_name[i])]
})
r_mon_spe <- NULL
r_2 <- NULL
  for(i in 2:length(file_name)){
  d <- rdata[[i]][month!='NA']
  t_r <- NULL  
    for (k in 1:nrow(d)){
    mon <- as.numeric(d[`k`,month]+match('mon_1',colnames(d)))-1
    temp <- as.numeric(d[`k`,mon,with=FALSE])
    t_r <- c(t_r,temp)
    }
  r2 <- cbind(d[,1:8],t_r,sub('.csv','',file_name[i]))
  r_mon_spe <- rbind(r_mon_spe,r2)
  }
write.csv(r_mon_spe,paste0(path,'/result_',file_name[i]))
  cat(i)
###############



####################plot
library(data.table)
  library(agricolae)
plot_data <- fread(paste0(path,"result_and_month_fin.csv"))
  aov1 <- aov(temp~name,data=plot_data)
  tuk <- scheffe.test(aov1,"name")
  aov_result <- summary.aov(aov1)
write(aov_result,paste0(path,"result_aov.csv"))  
  order_1 <-as.data.table(tuk$groups)
  order_1[,name:=rownames(tuk$groups)]
  name <-sort(rownames(tuk$groups),decreasing = T)
  plot_data <- plot_data[order_1,on=.(name=name)]
library(ggplot2)
ggplot(data=plot_data,aes(x=temp/10,y=name,fill=groups))+
  geom_boxplot()+
  labs(x="Mean monthly temperature (°C)",y="",fill='Group')+
  scale_fill_brewer(palette="GnBu")+
  scale_y_discrete(limits=name)+
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=13,face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))

ggsave(paste0(path,'prim_plot_c3.jpeg'),width = 10, height = 5,dpi = 600)
