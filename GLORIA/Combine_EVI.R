Sys.setlocale("LC_TIME", "English") 
#setting the Default language to eng, 
#making the abbreviation of month can be read.

comb_evi <- function(name,path){
  require(data.table)
  require(imputeTS)
  require(readr) 
    evi_t <- fread(paste0(path,name,"/Terra_16D.csv"))
    evi <- evi_t
    colnames(evi)<-c('time','EVI','AQ')
    evi_sel <- evi[,date:=as.Date(time,format="%b %d, %Y")][
      order(date)][AQ<=2,][
      ,EVI:=parse_number(EVI)/10000][
        EVI!=""]
    evi_sel[,EVI.sm:=loess(EVI~as.numeric(date),data=evi_sel,span=0.05)$fit]
    assign(paste0(name,"_EVI"),evi_sel)
    }
path <- "C:/Users/joker/Google 雲端硬碟/GLORIA_個人處理/EVI/total/"
######deal the EVI of SYU
name_list <- c('SEN','SUN','YAT','DSH',"JNJ","TSW")
for (i in 1:length(name_list)){
  name <- name_list[i]
  a <- comb_evi(name,path) 
  write.csv(a,paste0(path,'result/',name,'_EVI.csv'))
  assign(paste0(name,'_EVI'),a)
}

#########################deal the data
library(ggplot2)
library(data.table)
ggplot()+
  geom_line(data=DSH_EVI,aes(x=date,y=EVI.sm,color='DSH'))+
  geom_line(data=JNJ_EVI,aes(x=date,y=EVI.sm,color='JNJ'))+
  geom_line(data=TSW_EVI,aes(x=date,y=EVI.sm,color='TSW'))+ 
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("EVI")+
  labs(color = "Summit")+
  theme_classic()
 ggsave(filename = paste0(path,'/plot/SYU_terra_EVI.jpeg'),width = 7,height = 3,dpi = 600)
#########################################################
######################### 算EVI 年均值and季節值 備用  
DSH_EVI[,summit:='DSH']
JNJ_EVI[,summit:='JNJ']
TSW_EVI[,summit:='TSW'] 
r.EVI <- rbind(DSH_EVI,JNJ_EVI,TSW_EVI) 
r.EVI[,year:=year(date)][,month:=month(date)]
r.EVI[,month:=month(date)]
r.EVI[month%in%3:5,season:='Spring'][
  month%in%6:8,season:='Summer'][
    month%in%9:11,season:='Fall'][
      is.na(season),season:='Winter']
r.EVI[,year.s:=year][month==12,year.s:=year+1]
r.EVI.y <- r.EVI[,.(E.avg=mean(EVI),E.max=max(EVI),E.min=min(EVI)),by=.(year,summit)]
r.EVI.s <- r.EVI[,.(E.avg=mean(EVI),E.max=max(EVI),E.min=min(EVI)),by=.(year.s,summit,season)]
write.csv(r.EVI.s,"E:/忍者/GLORIA_個人處理/2020/SYU_EVIandcliment/EVI_season.csv")
ggplot(r.EVI.y,aes(x=year,y=E.avg,color=summit))+
  geom_line()
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("EVI")+
  labs(color = "Summit")+
  theme_classic()
#####plot season EVI
s <- c('Winter',"Spring",'Summer','Fall')
for (i in 1:4){
    ggplot(r.EVI.s[season==s[i]],aes(x=year.s,y=E.avg,color=summit))+
    geom_line()+
    scale_x_continuous(breaks = seq(2003,2020,2))+
    xlab("Year")+
    ylab("EVI")+
    labs(color = "Summit")+
    theme_classic()
  
  ggsave(filename = paste0(path,'/plot/SYU_EVI_season_',s[i],'.jpeg'),
         width =6,height = 4,dpi = 300)
  }
##########################################################
######### time series
library(imputeTS)
W <- as.data.table(seq(as.Date('2003-01-01'),as.Date('2020-06-30'),by='weeks'))
W[,yyww:=paste0(year(V1),'-',week(V1))] 
setkey(W,yyww)
for(i in 1:3){
  #### Increasing data resolution to weekly
  rd <- fread(paste0(path,'/result/',name_list[i],'_EVI.csv')) 
  rd[,yyww:=paste0(year(date),'-',week(date))]
  setkey(rd,yyww)
 rd_w <- rd[W][order(i.V1)]
 rd_w[,EVI_rNA:=na_interpolation(EVI)]
 rd_w[,EVI_wsm:=na_interpolation(EVI.sm)]
 rd_ts <- ts(rd_w[,EVI_wsm],frequency =52,start = c(2003,1))
 ts_r = decompose(rd_ts, "additive")

 r <- cbind(rd_w[,.(date,yyww,i.V1,EVI_rNA)],ts_r$x,ts_r$seasonal,ts_r$trend,ts_r$random)
 setnames(r,c('i.V1','V2','V3','V4','V5'),c('date_w','EVI','Season','trend','random'))
 assign(paste0(name_list[i],'_ts_r'),r)
 write.csv(r,paste0(path,'ts_result/',name_list[i],'_ts_r.csv'))
 }
##### plot the ts_result
library(ggplot2)
library(scales)
  ggplot()+
  geom_line(data=DSH_ts_r,aes(date_w,trend,color='DSH'))+
  geom_line(data=JNJ_ts_r,aes(date_w,trend,color='JNJ'))+
  geom_line(data=TSW_ts_r,aes(date_w,trend,color='TSW'))+
  scale_x_date(breaks="year",date_labels="%y")+
  labs(x='Year',y='Trend',color = "Summit")+
  theme_classic()
ggsave(filename = paste0(path,'/plot/SYU_trend_EVI.jpeg'),width = 7,height = 3,dpi = 600)

ggplot()+
  geom_line(data=DSH_ts_r[year(date_w)==2010],aes(date_w,Season,color='DSH'))+
  geom_line(data=JNJ_ts_r[year(date_w)==2010],aes(date_w,Season,color='JNJ'))+
  geom_line(data=TSW_ts_r[year(date_w)==2010],aes(date_w,Season,color='TSW'))+
  scale_x_date(date_breaks = 'month',labels = date_format("%b"))+
  labs(x='Month',y='Seasonal',color = "Summit")+
  theme_classic()
  
ggsave(filename = paste0(path,'/plot/SYU_season_EVI.jpeg'),width = 6,height = 3,dpi = 600)

ggplot()+
  geom_line(data=DSH_ts_r,aes(date_w,random,color='DSH'))+
  geom_line(data=JNJ_ts_r,aes(date_w,random,color='JNJ'))+
  geom_line(data=TSW_ts_r,aes(date_w,random,color='TSW'))+
  scale_x_date(breaks="year",date_labels=format("%y"))+
  labs(x='Year',y='Random',color = "Summit")+
  theme_classic()
ggsave(filename = paste0(path,'/plot/SYU_random_EVI.jpeg'),width = 7,height = 3,dpi = 600)
