library(imputeTS)
library(data.table)
library(broom) #get the lm coefficient
library(ggplot2)
library(wesanderson)
library(ggpubr)
Sys.setlocale("LC_TIME", "English")
#######################################################
############ regression and filling the miss value of temp
era_path <- 'E:/忍者/GLORIA_個人處理/Weather_data/'

list_S <- list( c('SUN','YAT','SEN'), c('JNJ','DSH','TSW'))
reg_l <- c('DAS','SYU')

#the wdata is the Das_era, 
#inculding the datalogger daily avg., max, and min temp. 
#Also, combining the ERA5 daily temp.
m_fr <- NULL
w_result <- NULL
r_cof <- NULL
for(k in 1:2){
    reg <- reg_l[k]
  S <- list_S[[k]]
  wdata <- fread(paste0(era_path,reg,'/cobim_w_era_data.csv'))  
  era5_d <- fread(paste0(era_path,'/',reg,'_ERA5_daily.csv'))
for (i in 1:3){

  rd <- wdata[summit==S[i]]
  dir <- unique(rd[,direction])
  for (j in 1:length(unique(dir))){
  
        r2 <- rd[direction==dir[j],]
   # r2[,i.temp_q:=i.temp^2]
    
    m_t <- lm(temp~i.temp,data=r2)
    summary(m_t)
    cof <- summary(m_t)$coefficients
    cof <- cbind(cof,summit=S[i],dir=dir[j])
    
    ######save the model coefficient
    m_r <-cbind(data.table(model="Temp",summit=S[i],direction=dir[j]),glance(m_t)) #glance from the package "broom"
    summary(m_t)
    r_cof <- rbind(r_cof,cof)
    m_fr <- rbind(m_fr,m_r)
    ################ merge the rdata and predict
    era5_d[,date:=as.Date(date)]
    r2[,date:=as.Date(date,format='%Y/%m/%d')]
    pre <- r2[,1:6][era5_d,on=.(date=date),all=T]
    
    #setnames(pre,c('temp.y','max_t.y','min_t.y'),c('i.temp','i.max_t','i.min_t'))
    pre[,temp.p:=predict(m_t,pre)]
    pre[,summit:=S[i]][,direction:=dir[j]][,region:=reg]
    pre[is.na(temp),c('temp','type'):=.(temp.p,"p") ]
    pre[,c(1,7,10:12):=NULL]
    w_result <- rbind(w_result,pre)
    
  }#finished j looping
} #i looping

}

write.csv(w_result,paste0(era_path,'資料填補基本結果/temp_combin_daily.csv'))
write.csv(r_cof,paste0(era_path,'資料填補基本結果/temp_model_coefficent.csv'))
write.csv(m_fr ,paste0(era_path,'資料填補基本結果/temp_model_coefficent_2.csv'))
##################################################`##########
ggplot(data=w_result[is.na(type)],aes(x=i.temp,y=temp))+
  geom_point()+
facet_grid(summit~direction)+
labs(x="ERA5 daily temperature (°C)",y='Soil temperature (°C)')
ggsave('E:/忍者/GLORIA_個人處理/paper_準備/plot/temperature_compare.jpeg',
       width=12,height=14,dpi=600)

count_p_day <- w_result[type=='p'&date>as.Date('2009-12-31')&date<as.Date('2019-12-31'),.(count=.N),by=.(summit,direction)]   
total_day <- as.Date('2019-12-31')-as.Date('2010-01-01')
fwrite(count_p_day,paste0(era_path,'資料填補基本結果/predict_day.csv'))
