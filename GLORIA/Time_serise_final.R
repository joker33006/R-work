##### time series analysis
library(data.table)
library(ggplot2)
library(imputeTS)
svpath <- "C:/Users/user/Desktop/GLORIA_EVI/ts_analysis_plot"
rdata <- fread("C:/Users/user/Desktop/GLORIA_EVI/EVI_Temp_pre_weekly.csv")
rdata <- rdata[!is.na(time),][,time:=as.Date(time)]
rdata_evi <- rdata[!is.na(time),][,.(SEN.sm,SUN.sm,YAT.sm,time)]
rdata_wea <- rdata[!is.na(time),][,.(sen_avg,sun_avg,yat_avg,pre_sum)]
ttend <- NULL
tseason <- NULL
trandom <- NULL
for (i in c(1:3)){
  ts_nd <- ts(rdata_evi[,i,with=FALSE],frequency =52,start = c(2003,1))
  #data.table need argument "with=FALSE" to make variable variables work
  deco_nd = decompose(ts_nd, "additive")
  tend <- as.data.table(deco_nd$trend)
  setnames(tend,"V1",colnames(rdata_evi)[i])
  season <- as.data.table(deco_nd$seasonal)
  setnames(season,"x",colnames(rdata_evi)[i])
  random <- as.data.table(deco_nd$random)
  setnames(random,"x - seasonal",colnames(rdata_evi)[i])
  ttend <- cbind(ttend,tend)
  tseason <- cbind(tseason,season)
  trandom <- cbind(trandom,random)
  summary(deco_nd)

  }
ttend[,time:=rdata$time]
tseason[,time:=as.Date(rdata$time)]
trandom[,time:=rdata$time]

#### weather time serise
for (i in c(1:4)){
  ts_nd <- ts(rdata_wea[,i,with=FALSE],frequency =52,start = c(2003,1))
  #data.table need argument "with=FALSE" to make variable variables work
  deco_nd = decompose(ts_nd, "additive")
  tend <- as.data.table(deco_nd$trend)
  setnames(tend,"V1",colnames(rdata_wea)[i])
  season <- as.data.table(deco_nd$seasonal)
  setnames(season,"x",colnames(rdata_wea)[i])
  random <- as.data.table(deco_nd$random)
  setnames(random,"x - seasonal",colnames(rdata_wea)[i])
  ttend <- cbind(ttend,tend)
  tseason <- cbind(tseason,season)
  trandom <- cbind(trandom,random)
  summary(deco_nd)
}
ttend[,time:=rdata$time]
tseason[,time:=as.Date(rdata$time)]
trandom[,time:=rdata$time]

###### Plot
library(ggplot2)
ggplot(rdata,aes(x=time))+
  geom_line(aes(y=SEN.sm,color="SEN"))+
  geom_line(aes(y=SUN.sm,color="SUN"))+
  geom_line(aes(y=YAT.sm,color="YAT"))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("EVI")+
  scale_color_discrete(name="Summit")
ggsave(paste0(svpath,"/EVI比較.jpeg"),width=7,height=3,dpi=600)

ggplot(ttend,aes(x=time))+
  geom_line(aes(y=SEN.sm,color="SEN"))+
  geom_line(aes(y=SUN.sm,color="SUN"))+
  geom_line(aes(y=YAT.sm,color="YAT"))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("trend")+
  scale_color_discrete(name="Summit") 

ggsave(paste0(svpath,"/EVI_trend_compare.jpeg"),width=7,height=3,dpi=600)

ggplot(tseason,aes(x=time))+
  geom_line(aes(y=SEN.sm,color="SEN"))+
  geom_line(aes(y=SUN.sm,color="SUN"))+
  geom_line(aes(y=YAT.sm,color="YAT"))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("seasonal")+
  scale_color_discrete(name="Summit")
ggsave(paste0(svpath,"/EVI_seasonal_compare.jpeg"),width=7,height=3,dpi=600)

ggplot(trandom,aes(x=time))+
  geom_line(aes(y=SEN.sm,color="SEN"))+
  geom_line(aes(y=SUN.sm,color="SUN"))+
  geom_line(aes(y=YAT.sm,color="YAT"))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("random")+
  scale_color_discrete(name="Summit")
ggsave(paste0(svpath,"/EVI_random_compare.jpeg"),width=7,height=3,dpi=600)
#####weather plot
ggplot(ttend,aes(x=time))+
  geom_line(aes(y=sen_avg,color="SEN"))+
  geom_line(aes(y=sun_avg,color="SUN"))+
  geom_line(aes(y=yat_avg,color="YAT"))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("random")


ggplot(ttend_m,aes(x=pre,y=sen.e))+
  geom_point()+
  xlab("temp trend")+
  ylab("EVI trend")
rdata_m  <- rdata[!is.na(time),][,.(SEN.sm,SUN.sm,YAT.sm,time)][,
                 yy:=year(time)][,mm:=month(time)][,yymm:=paste0(yy,mm)][
                   ,.(sen.e=mean(SEN.sm),sun.e=mean(SUN.sm),yat.e=mean(YAT.sm),year=min(yy),mm=min(mm)),by=yymm]
write.csv(rdata_m,paste0(svpath,"/EVI_monthly.csv"))
