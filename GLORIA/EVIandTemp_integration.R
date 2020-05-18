library(data.table)
library(imputeTS) #fix th na of EVI
path <- "C:/Users/user/Desktop/GLORIA_EVI"
evi_f <- list.files(paste0(path,"/EVI_integrated"))
temp <- fread(paste0(path,"/temp_lm_result/temp_predict_total.csv"))
pre <- fread(paste0(path,"/plot/Yat_rai.csv"))
pre[,yyww:=paste0(year(date),"-",week(date))]
pre_week <- pre[,.(pre_sum=sum(rai)),by=yyww]
temp[,yyww:=paste0(year(time),"-",week(time))]
temp_week <- temp[,.(sen_avg=mean(temp_sen_p),sen_max=max(temp_sen_p),sen_min=min(temp_sen_p),
                     sun_avg=mean(temp_sun_p),sun_max=max(temp_sun_p),sun_min=min(temp_sun_p),
                     yat_avg=mean(temp_yat_p),yat_max=max(temp_yat_p),yat_min=min(temp_yat_p),
                     time=min(as.Date(time))),
                  by=yyww]
w_TE <- temp_week
for (i in c(1:3)){
  EVI_D <-fread(paste0(path,"/EVI_integrated/",evi_f[i]))
  EVI_D[,yyww :=paste0(year(date),"-",week)]
  
  w_TE <- merge(w_TE,EVI_D,by="yyww",all=TRUE)[order(time),][,`:=`(V1=NULL,week=NULL)]
  w_TE[,`:=`(EVI=na_kalman(EVI),EVI.sm=na_kalman(EVI.sm))]
  sec_n <- substr(evi_f[i],1,3)
  setnames(w_TE,c("EVI","EVI.sm"),c(paste0(sec_n,".org"),paste0(sec_n,".sm")))
  
}
w_TE[,date:=as.Date(date)]
w_TRE <- merge(w_TE,pre_week,on=.(yyww=yyww))
w_TRE[,pre_sum:=pre_sum*1000]

write.csv(w_TRE,paste0(path,"/EVI_Temp_Pre_weekly.csv"))

